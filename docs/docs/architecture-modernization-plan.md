# ResGraph Architecture Modernization Plan

Status: Active; first safety and boundary milestone implemented
Reviewed revision: `dc1647b3f73a829b76aaf4399769ff18492a602b` (`v1.3.0`)
Review date: 2026-07-31

## Executive summary

ResGraph's fundamental design is sound and should be retained:

- ReScript compiler artifacts are the source of schema information.
- Native OCaml performs compiler-aware discovery and schema generation.
- ReScript/Node owns workspace orchestration, watch behavior, and editor transport.
- The generated schema uses `graphql-js` and exposes a small public ReScript surface.
- Each configured schema has independent outputs, diagnostics, and cache state.

The main architectural problem is concentration and coupling. Compiler ingestion,
declaration discovery, type resolution, validation, code generation,
authorization, artifact ownership, persistence, and editor support currently flow
through a few large modules and shared mutable or global state. This makes new
features expensive to add, makes some invalid states possible, and causes schemas
in the same repository to repeat expensive project discovery and CMT work.

The recommended direction is an incremental redesign around:

1. A compiler-root-level project index shared by every schema in that root.
2. A typed declaration index and canonical, immutable schema IR.
3. Explicit validation, resolver-planning, and authorization-planning passes.
4. ReScript, SDL, authorization, and tooling emitters that consume only frozen IR.
5. Transactional, manifest-owned generated artifacts.
6. A batch multi-schema engine coordinated by the ReScript CLI.
7. Compatibility, correctness, packaging, and performance gates for every stage.

This is not a proposal for a wholesale rewrite. The existing implementation
should be moved behind explicit boundaries, characterized, and replaced one phase
at a time. Mutable builders and hash tables should remain where they are the
fastest implementation; immutability is required at phase boundaries, not as an
ideological constraint inside every algorithm.

## Goals

- Retain every currently supported feature and public compatibility surface.
- Make it straightforward to add GraphQL features without editing several
  unrelated string printers and mutable registries.
- Improve multi-schema cold and watch performance by sharing compiler work.
- Preserve or improve single-schema cold, warm, and runtime performance.
- Make invalid schema states unrepresentable where practical and diagnose the
  remainder before generated GraphQL code is loaded.
- Make generation interruption- and failure-safe.
- Establish explicit ownership for generated files, caches, and editor indexes.
- Isolate ReScript compiler internals behind a narrow adapter.
- Make tests, release artifacts, documentation, and performance measurements
  reliable enough to support continuous development.

## Non-goals

- Rewriting the native generator in ReScript.
- Replacing the current code generator in one large change.
- Immediately splitting ResGraph into several npm packages.
- Adding a general plugin framework before a second real extension requires one.
- Parallelizing schema construction while generator state remains global.
- Adding a persistent native daemon before measurements show that process startup
  is a meaningful editor bottleneck.
- Trading predictable performance for a maximally pure internal implementation.

## Baseline and review validation

The review was performed in a clean worktree created from the latest
`origin/main` at the revision above. No production code was changed during the
review.

The following baseline checks were performed:

- Root ReScript compilation and CLI bundling succeeded.
- Cache, runtime, interface-validation, multi-schema, and authorization fixtures
  passed with the published v1.3.0 Linux native binary built from the same
  revision.
- A synchronous `ResGraph.Execute` query reproduced
  `execute(...).then is not a function`, confirming the `PromiseOrValue` binding
  issue.
- A clean packed-package consumer reproduced missing or accidental dependency
  behavior for package-root, DataLoader, and GraphQL imports.
- The tracked worktree remained clean after validation.

The native binary was not rebuilt locally because the review environment had no
selected opam switch. Before implementation starts, CI or a configured native
development environment should record a freshly built baseline from this exact
revision.

The current integration runner is not a fully trustworthy gate:
[`tests/test.sh`](../../tests/test.sh) does not enable strict shell failure handling,
so a failed runtime or interface-validation suite can be hidden by a later
successful command. Repairing this is the first prerequisite for structural work.

## Current architecture and primary constraints

The current native flow is approximately:

```text
CLI positional arguments
        |
        v
cache check and project/package discovery
        |
        v
source scan and CMT/CMTI loading
        |
        v
recursive type discovery + mutable declaration registration
        |
        v
interface inference/inheritance + validation
        |
        v
authorization planning
        |
        v
cleanup + interface/state/SDL/schema/auth/cache writes
```

The concentration is visible in the largest native modules:

- [`GenerateSchema.ml`](../../src/ml/GenerateSchema.ml): type mapping, recursive
  discovery, materialization, and resolver traversal.
- [`GenerateSchemaUtils.ml`](../../src/ml/GenerateSchemaUtils.ml): registration,
  source parsing, interface handling, validation helpers, conversions,
  diagnostics, state persistence, and filesystem I/O.
- [`GenerateSchemaTypePrinters.ml`](../../src/ml/GenerateSchemaTypePrinters.ml):
  runtime construction, generated helpers, interface modules, output cleanup, and
  writes.
- [`GenerateSchemaAuthorization.ml`](../../src/ml/GenerateSchemaAuthorization.ml):
  semantic planning, signature analysis, code emission, and filesystem handling.
- [`GenerateSchemaDirect.ml`](../../src/ml/GenerateSchemaDirect.ml): orchestration,
  cache policy, global hook installation, error branches, and output sequencing.

There is one monolithic native executable stanza and effectively no internal
`.mli` boundaries. The large mutable `schemaState` contains declarations,
work-in-progress relationships, diagnostics, source data, and emission metadata.

The current ReScript/Node flow is approximately:

```text
resgraph.json
    |
    v
unchecked/partially normalized config
    |
    v
one synchronous native process per schema or editor operation
    |
    v
Node and native code independently reason about artifact ownership
```

This repeats project work for schemas sharing a compiler root and duplicates
invariants across the process boundary.

## Immediate correctness and safety concerns

These issues should be fixed or characterized before the core refactor.

### Test runner can report false green

The root shell runner does not use `set -euo pipefail`. It also mutates fixtures,
configuration, symlinks, dependencies, caches, and generated output without a
single robust cleanup trap. Every suite must independently propagate failure, and
all mutations must be restored on success, failure, and interruption.

### Artifact writes are non-transactional

Generated source, signatures, interface helpers, tooling state, SDL,
authorization files, and cache entries are written in sequence. A watcher or LSP
request can observe a mixed generation, and an exception can leave a new state
file paired with old source.

On schema diagnostics, generation can replace the last-known-good schema with an
invalid `Obj.magic` placeholder. Legacy cleanup also deletes files based on name
patterns without consistently proving generated ownership.

### Schema registration is order-dependent

Declaration kinds use independent hash tables. Duplicate same-kind declarations
are often first- or last-wins, while cross-kind GraphQL name collisions can
survive until GraphQL.js processes the emitted schema.

`addInputUnion` currently checks the regular-union table instead of the
input-union table. A regular union can suppress an input union with the same
internal identity, while duplicate input unions are not detected correctly.

### Input and output type validity is not represented in the model

One `graphqlType` representation covers input types, output types, context and
resolve-info injection, interface typename injection, and synthetic sentinels.
Every consumer must remember which constructors are legal in its context. Some
invalid input/output placements and input-union member shapes can therefore reach
the generated schema instead of producing native, source-located diagnostics.

### Discovery and materialization are interleaved

Type lookup can resolve aliases, synthesize declarations, register authorization,
mutate schema state, add diagnostics, and recurse into child fields. A second
traversal contains similar object, interface, input, enum, union, and scalar
construction paths. Recursive types depend on incidental insertion order rather
than explicit visiting states.

### Runtime bindings overpromise safety

- `DataLoader.prime` and `primeWithPromise` omit the key argument.
- `DataLoader.loadMany` does not represent per-entry errors.
- The deep cache-key serializer sorts arrays, causing order-sensitive keys to
  collide.
- `ResGraph.Execute` lets callers select result types without a decoder or other
  proof.
- GraphQL execution is bound as always asynchronous even though GraphQL.js returns
  `PromiseOrValue`.
- Malformed variables JSON is silently treated as no variables.
- Query caching hashes the query twice on misses and has no size bound.
- Yoga exposes incompatible plugin abstract types.
- Input-union conversion silently selects a member and relies on an Envelop plugin
  for zero-or-one enforcement.

### Persisted editor state is coupled to internal OCaml records

The generator marshals much of its evolving mutable schema representation. A
manually maintained integer version does not make `Marshal` safe across record or
variant layout changes. Editor operations need a much smaller, explicit data
model.

### LSP behavior has correctness gaps

- Diagnostics use filesystem paths where LSP requires file URIs.
- Open `.resi` documents are not retained even though the completer supports them.
- GraphQL hover and definition ignore unsaved text.
- Temporary ReScript completion files are predictable and are not removed.
- Native process failures can leave stale diagnostics.
- Global server state prevents isolated tests and multiple server instances.
- Synchronous native calls block JSON-RPC handling.
- The extension uses only the first workspace folder and starts `npx resgraph`,
  which can select or download a version other than the workspace dependency.

### Packaging and release behavior is not tested as consumed

Tests compile against repository source rather than a packed tarball. A fresh
consumer currently sees a nonexistent package `main`, missing direct/peer
dependencies, and version resolution outside an explicitly tested compatibility
range. Release binaries are built through the default Dune profile even though a
release-optimized profile exists.

The package license metadata says ISC while the license file contains MIT text.
Vendored compiler code also needs an explicit provenance and notices document.

## Target architecture

```text
ReScript / Node
+----------------------------------------------------------------+
| Config.Raw -> Decode -> Normalize/Validate -> WorkspacePlan     |
|                                           |                    |
| BuildCoordinator -> CompilerWatcher -> EngineClient             |
+-------------------------------------------+--------------------+
                                            | versioned JSON
                                            v
Native OCaml
+----------------------------------------------------------------+
| ProjectIndex + SummaryStore, shared per compiler root           |
|                         |                                      |
|                         v                                      |
| DeclarationIndex -> per-schema Builder/Worklist                 |
|                         |                                      |
|                         v                                      |
|                 immutable SchemaIR                             |
|              /              |                \                  |
|       ResolverPlan     validation passes    AuthorizationPlan   |
|              \              |                /                  |
|      ReScript / SDL / authorization / editor-index emitters     |
|                         |                                      |
|                         v                                      |
|             ArtifactPlan -> atomic transaction                  |
+----------------------------------------------------------------+
```

### Dependency rule

Dependencies flow downward. Compiler types such as `Types.type_expr`, `Path.t`,
`Ctype`, and `Btype` must not escape the compiler adapter into schema IR,
validation, emitters, protocol DTOs, or tooling indexes.

### Native libraries

Split the executable into wrapped libraries with explicit `.mli` files:

#### `resgraph_base`

- `GraphqlName`
- `PathIdentity`
- `SourceLocation`
- `Diagnostic`
- shared error/result types
- protocol data transfer objects

#### `resgraph_compiler`

- `CompilerAdapter`
- `ProjectDiscovery`
- `ProjectIndex`
- `CmtReader`
- immutable `ModuleSummary`
- `SummaryStore`
- `TypeResolver`
- source index needed for supported legacy record-spread syntax

This is the only library that knows about the vendored ReScript compiler.

#### `resgraph_schema`

- collected declarations and provenance
- global GraphQL name registry
- typed builder/worklist
- canonical `SchemaIr`
- `InterfaceGraph`
- validation rule set
- `ResolverPlan`
- `AuthorizationPlan`
- value representation and codec planning

#### `resgraph_emit`

- ReScript emitter
- SDL emitter
- authorization emitter
- compact `EditorIndex` emitter
- `OutputPlan`
- atomic `ArtifactTransaction`

#### `resgraph_service`

- `WorkspaceSession`
- multi-schema build coordination
- invalidation and cache coordination
- profiling counters
- per-schema result isolation

#### `resgraph_cli`

A thin executable responsible for protocol decoding, dispatch, structured error
encoding, and process exit status.

### ReScript/Node modules

Split the current broad utility and LSP modules into:

- `Config.Raw`
- `Config.Decode`
- `Config.Normalized`
- `Config.Validate`
- `PathIdentity`
- `SchemaRouting`
- `WorkspacePlan`
- `EngineProtocol`
- `EngineClient`
- `BuildCoordinator`
- `CompilerWatcher`
- LSP transport, session state, and individual handlers

Legacy configuration should be adapted into the normalized model at one boundary
and remain supported for the documented compatibility window.

## Canonical schema model

### Separate GraphQL positions from ReScript representation

Introduce distinct types along these lines:

```text
InputType
OutputType
ResolverArgument =
  | Argument(InputType)
  | Context
  | ResolveInfo
  | InterfaceTypename

ValueRepresentation =
  | Plain
  | Option
  | Nullable
  | CustomCodec
```

GraphQL nullability belongs to the GraphQL type tree. ReScript `option` versus
`Nullable` belongs to conversion metadata. Context and resolve-info injection do
not belong to either GraphQL input or output types.

### Collect before materializing

Generation should be divided into explicit phases:

1. Discover packages, selected files, and CMT/CMTI inputs.
2. Summarize compiler data into immutable module summaries.
3. Collect every annotated declaration and resolver without traversing its full
   type graph.
4. Register GraphQL names globally, recording kind and source provenance.
5. Materialize reachable declarations through a worklist with `Unseen`,
   `Visiting`, and `Complete` states.
6. Derive interface closure, inherited fields, resolver plans, authorization
   plans, and codecs.
7. Validate the complete graph.
8. Freeze maps and arrays in canonical deterministic order.
9. Emit all requested outputs from frozen IR.
10. Commit the artifact transaction and then persist cache/editor indexes.

Registering placeholders before traversing children makes recursive and mutually
recursive declarations deliberate rather than dependent on incidental table
insertion.

### Centralize GraphQL semantics

Provide one implementation for:

- type equality and nullability
- input/output position validity
- subtype and interface compatibility
- GraphQL name validation
- SDL type rendering
- runtime GraphQL type folding
- interface ancestry and concrete implementor closure
- union and input-union membership
- oneOf constraints
- recursive non-null input-object cycle validation

Emitters should not independently rediscover these rules.

### Extending ResGraph

A typical new GraphQL feature should require:

1. Compiler-adapter recognition.
2. A declaration or canonical IR representation.
3. A validation rule where applicable.
4. Resolver, authorization, or codec planning where applicable.
5. Changes only to relevant emitters.
6. Unit, runtime, compatibility, and performance fixtures.

Use ordinary pass and emitter interfaces. Introduce a generalized extension API
only when a concrete second implementation demonstrates the required abstraction.

The canonical IR also establishes a clean future input for an operation compiler,
`graphql-ppx` integration, or Sury/JSON Schema operation-codec generation without
forking GraphQL type rules.

## Artifact ownership and transactions

Introduce one versioned artifact manifest. Every entry contains:

- canonical path
- artifact kind
- owning schema identity
- content digest
- format version

The transaction is:

1. Render all requested artifacts in memory or bounded staging buffers.
2. Validate the complete result and ownership plan.
3. Write changed files to same-directory temporary siblings.
4. Rename each staged file atomically.
5. Remove only files proven owned by the prior manifest and carrying the expected
   generated marker.
6. Commit the new manifest last.
7. Persist the cache and tooling index only after source output succeeds.

On a failed rebuild, preserve the last-known-good artifact set. A first-ever
invalid build may create a clearly marked compile-safe bootstrap stub if needed,
but the command must still fail and CI must not silently execute a stale schema.

Only a verified missing-file error should be ignored. Permission, symlink,
rename, read, write, digest, and manifest failures must be structured diagnostics.

## Multi-schema build model

Normalize all configuration into a `WorkspacePlan` and group schemas by canonical
ReScript compiler root.

For each root:

1. Watch the compiler build marker once.
2. Check all selected schema caches.
3. If every schema hits, return without loading CMTs.
4. Build one project/package index for all misses.
5. Scan the union of selected source roots once.
6. Load and summarize each CMT/CMTI at most once.
7. Construct each schema independently over shared immutable summaries.
8. Return a result per schema so diagnostics and failure isolation are retained.
9. Commit successful schema transactions independently.

Initially build schemas sequentially over shared summaries to cap peak memory.
Distinct compiler roots may use bounded parallelism after global state has been
removed. Watch mode should allow at most one active generation and one coalesced
trailing rebuild per root.

## Cache model

Keep per-schema cache decisions while sharing summary and filesystem-signature
work within a compiler-root batch.

Each schema cache should include:

- normalized schema configuration
- tool and compiler-adapter build identifiers
- selected source roots and membership signatures, so added files invalidate
- source, CMT, and CMTI files actually needed for type and policy resolution
- relevant project and dependency configuration
- transitive module resolution closure
- authorization policies and required baseline inputs
- exact generated output digests

Cache missing module/CMT lookups as well as successful ones during a build.
Memoize path identity and filesystem signatures across schema decisions.

Authorization should be represented as ordinary inputs and outputs so required or
baseline authorization no longer disables caching wholesale.

## Generated runtime and FFI

Representative generated schemas repeat raw JavaScript helpers, unsafe recursive
type cells, property closures, and absent optional configuration fields.

Introduce a private `ResGraph__GeneratedRuntime` containing:

- opaque, one-shot recursive GraphQL type cells
- source representation unwrapping
- input conversion helpers
- abstract type discrimination
- shared property resolution
- contextual invariant failures

Use GraphQL.js's default field resolver wherever it has exactly the required
semantics. Distinguish default properties, renamed or representation-aware
properties, and true function resolvers in `ResolverPlan`.

Replace open-object FFI with typed records and dictionaries where the upstream
shape is stable. Keep unsafe casts private, narrow, and explicitly named. Preserve
the generated public `.resi` surface, especially `let schema`, while changing
implementation details.

Do not change `CodeWriter` or property-access strategies based on intuition alone;
benchmark construction, compilation, heap use, and query throughput first.

## Public runtime evolution

Add a safe execution API that:

- parses and validates operations by default
- normalizes GraphQL `PromiseOrValue` through `Promise.resolve`
- returns a fixed JSON GraphQL envelope or requires an explicit decoder
- reports malformed variables as a domain error
- uses a bounded query cache keyed directly by query string

Keep the current generic execution surface as deprecated or explicitly `Unsafe`
until a major release permits removal.

Correct the DataLoader bindings and expose per-entry `loadMany` failures as
`result`. Make identity keys the normal low-cost API and offer explicit deep/JSON
key behavior separately. Preserve array order in canonical serialization.

Unify Yoga plugin types, consolidate GraphQL error bindings, expand safe
`ResolveInfo` accessors, and retain one clearly named unsafe escape hatch rather
than encouraging arbitrary application casts.

## LSP and extension plan

### Correctness first

- Convert filesystem paths to real file URIs.
- Retain unsaved `.res`, `.resi`, and GraphQL document content.
- Clean temporary files in guaranteed finalizers, or parse buffers directly.
- Clear or replace diagnostics deterministically after process failures.
- Attribute and deduplicate shared-source diagnostics by schema.
- Accept numeric and string JSON-RPC IDs.
- Contain handler exceptions and return proper protocol errors.

### Structure

- Move global state into `LspServer.t`.
- Separate transport, document storage, schema routing, diagnostics, completion,
  hover, and definition handlers.
- Remove inherited protocol surface that ResGraph does not support.
- Replace raw full-schema persistence with a compact versioned `EditorIndex`.
- Use asynchronous engine calls and support cancellation.

### Later, measurement-gated improvements

- Persistent native editor worker with indexes invalidated by compiler markers.
- Configuration reload and watcher reconciliation.
- True multi-root workspace support.
- Deterministic workspace-local CLI resolution.
- Extension/engine version and protocol handshake.

The current tiny warm native process measurement is approximately 5 ms per hover
invocation. Persistent-worker complexity should be gated on representative large
projects and p50/p95 editor measurements.

## Test strategy

### Native unit tests

Add table-driven tests for:

- GraphQL names and duplicate registration
- input/output type construction and nullability
- recursive builder visiting states
- interface ancestry, inheritance, and cycles
- union and input-union membership
- config decoding and normalization
- source and path identity
- cache decisions and dependency closure
- authorization planning
- artifact plans and ownership migration
- diagnostic structure and ordering
- string, description, deprecation, and SDL escaping

### Runtime behavior

Exercise generated schemas rather than only inspecting emitted strings:

- synchronous and asynchronous queries
- mutations
- subscriptions and cancellation
- objects, interfaces, unions, recursive types, and abstract resolution
- input objects and every zero/one/multiple input-union shape
- custom scalar parse/serialize and `specifiedBy`
- connections and pagination
- authorization allow/deny, sync/async policy, ordering, and error propagation
- DataLoader batching, cache identity, `prime`, `loadMany`, and failures
- Execute parse, validation, variables, caching, data, and errors
- Yoga plugin integration

### Differential compatibility

While replacing the builder or emitters, compare old and new paths using:

- normalized diagnostics
- generated public module/signature inventory
- byte-level output where the stage promises no output changes
- intentionally reviewed focused goldens where formatting changes
- normalized SDL
- normalized GraphQL introspection
- black-box operation results

### Filesystem and cache fault tests

- interrupted and failed writes
- permission failures
- truncated cache/state/manifest files
- unsupported format versions
- concurrent invocation
- output tampering
- symlink retargeting and aliases
- added, removed, renamed, and moved source files
- schema ownership transfer
- generated-looking user files
- missing output directories
- configuration and authorization baseline changes

### LSP protocol tests

Replace fixed timers with a client that waits for response IDs and notifications.
Cover initialization ordering, diagnostics, completion, hover, definition,
unsaved buffers, `.res`, `.resi`, GraphQL files, shared-source routing, numeric
IDs, cancellation, process failure, rebuilds, and config reload.

### Package consumer tests

Pack the exact release candidate into a fresh temporary consumer with no access to
the repository's `node_modules`, then:

- install it using the supported package manager
- compile a ReScript consumer
- run `resgraph build`
- import and execute the generated schema
- exercise DataLoader
- exercise optional Yoga integration when installed
- verify expected compiler metadata and native platform binary
- reject unsupported platform/architecture combinations explicitly

Run against the minimum supported and newest supported dependency versions.

## Performance plan

Add `--profile-json` or an equivalent structured profiler reporting:

- project discovery duration
- source scan duration and file count
- CMT read/summarize duration and count
- declaration collection and materialization duration
- validation and planning duration
- each emitter's duration
- cache hit/miss reason and validation cost
- filesystem signature and write counts
- peak live/heap words or RSS

Benchmark:

- 1, 5, and 20 schemas sharing one compiler root
- 1, 5, and 20 schemas across separate roots
- small and representative large projects
- cold build
- warm no-op cache hit
- common-source change
- schema-local change
- dependency change
- authorization policy/baseline change
- generated schema compilation and construction
- representative query throughput
- LSP completion and hover p50/p95 while idle and rebuilding

Initially record results without blocking changes. Once environmental variance is
understood, set regression budgets. Single-schema cold and warm behavior should
not regress by more than roughly 5-10% without an explicit, reviewed reason.
Multi-schema builds should demonstrate a material improvement in process count,
CMT reads, filesystem work, and wall time. Peak RSS must remain bounded; prefer
sequential schema construction over shared summaries if parallel construction
causes excessive memory growth.

## Compatibility contract

Every stage must preserve or explicitly migrate:

- GraphQL semantics and normalized introspection
- queries, mutations, subscriptions, scalars, objects, interfaces, unions, input
  unions, connections, and authorization
- generated public module names and signatures
- existing interface helper modules
- `let schema` in generated `.resi` files
- legacy and named-schema configuration
- CLI commands and meaningful exit status
- first-schema default routing behavior
- multi-schema routing and per-schema failure isolation
- output paths and generated ownership
- authorization manifests and baselines
- named-schema cache/state locations or a documented migration
- completion, hover, definition, and diagnostics
- supported ReScript, Node, GraphQL, operating-system, and architecture ranges

Use adapters and deprecation periods for public API changes. Internal cache or
tooling formats may invalidate and rebuild when their version changes, but failure
must be explicit and safe.

## Delivery stages

### Stage 0: trustworthy baseline

- Repair strict test propagation and cleanup.
- Add missing runtime, native unit, package-consumer, and LSP protocol coverage.
- Record generated ABI, introspection, feature, and performance baselines.
- Add the structured phase profiler.

Exit criteria: every current feature has an executable characterization test, the
packed artifact is tested, and failures cannot be hidden by shell sequencing.

### Stage 1: correctness and release fixes

- Fix Execute `PromiseOrValue`, validation, variables, and cache behavior.
- Fix DataLoader, Yoga plugin types, input-union registration/enforcement, and
  duplicate GraphQL names.
- Fix LSP URI, `.resi`, temp-file, unsaved-buffer, and stale-diagnostic behavior.
- Fix dependency/peer declarations, package entry points, license metadata, CLI
  status/version output, and release-profile binary production.

Exit criteria: all known P0 correctness issues have regression tests and packed
consumers use only declared dependencies.

### Stage 2: infrastructure boundaries

- Extract native libraries and `.mli` interfaces.
- Introduce normalized config, `WorkspacePlan`, `PathIdentity`, typed errors,
  structured protocol DTOs, and `GenerationContext`.
- Remove global package/resolver hooks and deep exits.
- Preserve generated output byte-for-byte.

Exit criteria: compiler internals are isolated, generation is callable as a
library, and the old generator runs behind the new boundaries.

### Stage 3: canonical IR and passes

- Add declaration collection and the explicit worklist builder.
- Split input, output, resolver argument, and value-representation types.
- Add global name registration and centralized GraphQL semantics.
- Extract interface, validation, resolver, and authorization passes.
- Dual-run old and new implementations in tests.

Exit criteria: diagnostics, public ABI, introspection, execution, and measured
performance meet the compatibility gates.

### Stage 4: emitter and artifact separation

- Make every emitter consume frozen IR only.
- Introduce `OutputPlan`, atomic writes, exact manifest ownership, and last-known-
  good behavior.
- Replace internal-state `Marshal` with a versioned `EditorIndex`.
- Centralize escaping and generated runtime helpers.

Exit criteria: fault injection cannot produce partial mixed generations or remove
unowned files.

### Stage 5: shared multi-schema engine and scoped cache

- Add batch requests grouped by compiler root.
- Share discovery, source indexing, summaries, resolution, and signatures.
- Retain per-schema result and artifact isolation.
- Track schema-local dependency closure.
- Enable caching for authorization modes.
- Coalesce watch rebuilds.

Exit criteria: single-schema performance stays within budget and multi-schema
benchmarks demonstrate material improvement.

### Stage 6: measured hot-path and editor optimization

- Optimize file discovery, interface sets, diagnostic deduplication, authorization
  signature reuse, and sorting-on-freeze.
- Specialize generated representation unwrapping and property resolution only
  where benchmarks show benefit.
- Add a persistent native editor worker only if representative measurements
  justify it.
- Add config reload and multi-root extension support.

Exit criteria: each optimization has a targeted benchmark and retains behavioral
parity.

### Stage 7: public API evolution and project hygiene

- Add safe Execute and ResolveInfo APIs and deprecate unsafe surfaces.
- Publish config JSON Schema and compatibility matrices.
- Complete architecture, contributor, generated-artifact, performance, and release
  documentation.
- Test documentation snippets and the example project.
- Reconsider platform-specific binary packages only if measured installation size
  justifies the release complexity.

## Recommended pull-request sequence

Keep changes reviewable and preserve a working main branch throughout:

1. Strict tests, cleanup traps, and baseline measurements.
2. Runtime and LSP correctness fixes plus packed-package consumer tests.
3. Artifact ownership manifest and transactional writes.
4. Native library boundaries, explicit errors, and `GenerationContext`.
5. Declaration index, global name registry, and typed IR in dual-run mode.
6. Validation/resolver/authorization passes and emitter migration.
7. Stable editor index and removal of raw internal-state persistence.
8. Compiler-root batching and schema-local cache invalidation.
9. Generated-runtime and measured hot-path improvements.
10. Public safe APIs, documentation, and removal of deprecated internals when the
    compatibility window permits it.

Each pull request should state which invariant it establishes, include its own
characterization or regression tests, and attach relevant profiler output.

## Documentation required for continuous extension

Add and maintain:

- system and data-flow architecture
- canonical IR invariants
- compiler-adapter and supported ReScript-version policy
- generated-artifact ownership and transaction rules
- cache correctness and invalidation invariants
- CLI/native protocol specification
- public and generated ABI compatibility policy
- performance methodology and baseline storage
- an "adding a GraphQL feature" checklist
- contributor setup and deterministic root task surface
- release and vendored-compiler update runbooks
- short architecture decision records for lasting choices

Getting Started should double as a packed-package consumer fixture. Selected code
snippets should compile in CI, broken documentation links should fail the docs
build, and the checked-in example must be regenerated and tested.

## Risks and mitigations

### Generated output is effectively a public API

Use byte-level fixtures where output must remain unchanged, focused reviewed
goldens where formatting intentionally changes, and public module/signature
inventory checks everywhere.

### Compiler internals are version-sensitive

Confine all compiler-specific values and operations to `resgraph_compiler`. Test
each supported ReScript version through the adapter contract.

### Narrower cache inputs can become unsound

Track selected source membership plus actual transitive resolution dependencies.
Add invalidation fuzzing and retain output digest verification.

### Batching can raise memory use

Share immutable summaries, but build and freeze schemas sequentially initially.
Introduce bounded parallelism only after profiling.

### Artifact migration is destructive if ownership is wrong

Introduce exact manifest ownership before changing layouts. Verify markers and
digests, test symlinks and user-owned collisions, and preserve last-known-good
outputs.

### Runtime representations depend on ReScript output details

Guard representation-aware optimizations with black-box object, interface, union,
input-union, and recursive-value execution tests.

### A broad rewrite could obscure performance regressions

Move existing code behind interfaces first, dual-run new phases, and remove old
paths only after compatibility and profiling gates pass.

## First implementation milestone

The first milestone should combine immediate safety with prerequisites for later
work:

1. Make the test suite and packed-package validation trustworthy.
2. Record feature, ABI, introspection, and performance baselines.
3. Fix known Execute, DataLoader, Yoga, input-union, name-registry, LSP, and
   packaging correctness issues.
4. Introduce transactional artifact ownership and last-known-good behavior.
5. Extract native library boundaries and explicit `GenerationContext` while
   retaining the existing generator behind them.

Only after that milestone should the canonical IR replace the existing builder.
Compiler-root batching should follow the IR and context work, because those
boundaries allow CMT summaries to be shared safely and make multi-schema
performance improvements straightforward rather than entangled with global state.
