---
sidebar_position: 30
---

# Architecture and contributor guide

ResGraph deliberately splits work between ReScript/Node and native OCaml. The
Node CLI owns configuration, workspace routing, watch coordination, editor
transport, and generated-file ownership. The native engine owns compiler
metadata, GraphQL declaration discovery, semantic validation, authorization
planning, and schema emission.

```text
resgraph.json
    |
    v
Node config normalization and schema routing
    |
    v
native generation context and compiler summaries
    |
    v
schema discovery -> validation/planning -> emitters
    |
    v
generated ReScript, SDL, authorization, cache, and editor artifacts
```

The native executable is intentionally thin. Generator code lives in the
wrapped `resgraph_engine` library, and `GenerationContext` is the explicit home
for reusable CMT and module-summary caches. Normal builds group schemas by
canonical compiler root and send one length-prefixed batch request per root.
Schemas build sequentially with aligned results: compiler summaries are shared
within their owning package, while mutable schema state, artifacts, diagnostics,
and failures remain isolated.

## Invariants

- Every configured schema has independent output, cache, diagnostics,
  authorization, and editor-state identity.
- Source inclusion is explicit for named schemas. Generated output folders are
  excluded from subsequent discovery.
- Failed validation preserves the last successfully generated schema. A
  compile-safe bootstrap is created only when no prior schema exists.
- Generated-looking filenames are not proof of ownership. Cleanup requires the
  ResGraph generated marker or the configuration ownership manifest.
- Changed native artifacts are written to same-directory temporary siblings and
  atomically renamed. Cache and editor state are persisted only after source
  output succeeds.
- GraphQL operations executed through `ResGraph.Execute` are validated before
  execution and normalized to promises whether GraphQL.js completes
  synchronously or asynchronously.
- `DataLoader.makeBatchedResults` and `loadManyResults` preserve per-key
  failures as `result` values. Use `primeAt` and `primeWithPromiseAt` for
  keyed priming; the old keyless signatures remain only for source
  compatibility and are deprecated.
- Compiler-specific values stay inside the native engine. Public runtime APIs
  expose ReScript types, GraphQL values, JSON, results, and promises.

## Adding a GraphQL feature

Keep recognition, semantics, and printing separate. A feature normally needs:

1. Attribute or compiler-type recognition in the native discovery layer.
2. An explicit representation in `GenerateSchemaTypes` (and eventually the
   canonical schema IR described in the modernization plan).
3. Source-located validation in `GenerateSchemaValidation`.
4. Resolver, conversion, or authorization planning where applicable.
5. Changes only to the relevant ReScript and SDL emitters.
6. A native unit fixture plus a generated-schema runtime fixture.
7. Compatibility checks for generated signatures, normalized SDL or
   introspection, and operation results.

Do not add a printer-only special case for a semantic rule. If both SDL and
runtime code need to understand it, give it one representation and one
validation rule first.

## Generated artifacts and persistence

Named-schema ownership is recorded in
`lib/resgraph/.configured-schemas.json`. Only marker-owned files from a previous
configuration are eligible for cleanup. Native authorization manifests have
their own marker and failure status.

The current editor index is a versioned native state file. Treat its contents as
private and disposable: incompatible versions must ask the user to rebuild. A
future migration will replace the broad marshalled state with a compact,
versioned DTO; no external tool should read the existing format.

Incremental cache correctness takes priority over hit rate. A cache key includes
configuration, compiler/runtime selection, project and dependency inputs,
compiled metadata, and output integrity. New dependencies must first be tracked
conservatively; narrowing invalidation requires a fault-oriented regression
test.

## Native protocol

The Node CLI invokes the native executable with private positional commands.
Stdout is reserved for one JSON response and stderr for operational failures or
diagnostic presentation. Existing command shapes are compatibility surfaces for
the published Node CLI even though they are not a user-facing API.

Language-server request IDs are opaque passthrough values so both numeric and
string JSON-RPC IDs survive unchanged. Handler failures return an internal-error
response without terminating the server.

When evolving the native protocol, introduce a versioned request/response DTO,
retain per-schema results, and update package-consumer and LSP tests together.
Avoid adding process exits below the CLI boundary.

## Performance work

Measure before changing representation or adding a persistent process. Relevant
scenarios are cold generation, warm cache hits, shared-source changes, and 1, 5,
and 20 schemas per compiler root. Record CMT reads, source scans, writes, wall
time, and peak memory. Generated-schema construction and representative query
throughput are separate benchmarks from generator speed.

Single-schema behavior is the compatibility baseline. Multi-schema optimization
should reduce process count and repeated CMT work while building schemas
sequentially over shared summaries; bounded parallelism remains measurement
gated.

## Local validation

From the repository root, use `make test` for the native build and integration
fixtures. Also run `npm test`, `npm run test:package`, and `npm run build` when
changing public runtime bindings or packaging. The packed-package test installs
the tarball into a clean consumer, compiles a schema, runs a query, and exercises
DataLoader.

The full staged roadmap, compatibility gates, and rationale live in the
[architecture modernization plan](architecture-modernization-plan).
