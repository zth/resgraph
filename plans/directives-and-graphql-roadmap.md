# Directives and GraphQL capability roadmap

Status: directive foundation implemented on `agent/directives-roadmap`; remaining
directive and capability work is tracked below
ResGraph baseline: `origin/main` at `dc1647b` (`v1.3.0`, 2026-07-30)
Research date: 2026-07-31

## Executive summary

ResGraph should treat directives as schema data, not as executable decorators.
The compiler should understand directive definitions and ordered directive
applications, validate them, and lower them consistently into both generated
SDL and the generated `graphql-js` schema. Implementing directive behavior
should remain an explicit schema-transform or server-plugin concern.

The cleanest ReScript-facing design is:

- Define a directive with `@gql.directive` on an abstract or record type. A
  record models the directive's typed arguments; an abstract type defines a
  directive with no arguments.
- Apply a directive with repeatable `@gql.annotate` attributes whose payload is
  a GraphQL-const-compatible ReScript record.
- Preserve applied directives at runtime in two projections generated from the
  same ordered IR: the ecosystem-compatible `extensions.directives` map and an
  exact ordered list under `extensions.resgraph.appliedDirectives`.
- Keep built-ins semantic: lower `@deprecated`, `@specifiedBy`, and inferred
  `@oneOf` to their native `graphql-js` properties as well as correct SDL.
- Add one shared input-value/constant-value model to the IR. Directives,
  argument defaults, input-field defaults, descriptions, and deprecations then
  build on the same foundation.

The GraphQL runtime contract is now modernized: ResGraph peers on
`graphql@^16.11 || ^17`, tests latest v16, and emits native `isOneOf` input
objects without the obsolete validation plugin. OneOf is part of the September
2025 GraphQL specification.

After this stacked branch, the two substantial user-visible Grats gaps are
generic type materialization and derived context providers. The remaining
general GraphQL work is intentionally integration-led: explicit external/type
extensions, optional Federation support, and source-remapped validation for
schemas whose SDL is not persisted. Defaults, argument metadata, async list
inference, root shorthand, schema definitions, scalar coercion, and the full
type-system directive location set are delivered here.

### Implementation status

The directive foundation delivered on this branch includes:

- Typed custom directive definitions from abstract and record types, including
  locations, repeatability, descriptions, typed arguments, defaults, and
  deprecated arguments.
- Ordered applications on scalars, objects, interfaces, unions, enums, enum
  values, input objects, input fields, and output fields.
- Shared GraphQL constant-value parsing/coercion and build-time validation for
  definitions, locations, repeatability, arguments, and values.
- SDL and executable-schema parity, custom directive introspection,
  `specifiedByURL`, the GraphQL Tools `extensions.directives` convention, and
  an exact ordered `extensions.resgraph.appliedDirectives` projection.
- Native OneOf input objects, introspection, and coercion on the supported
  graphql-js range without an Envelop validation plugin.

The stacked roadmap branch additionally delivers:

- A schema marker for descriptions, root mappings, and `SCHEMA` applications.
- Resolver source parameters are correlated with CMT output, so
  `ARGUMENT_DEFINITION` applications, defaults, descriptions, and deprecations
  use normal parameter syntax. ReScript requires `@gql.description` rather
  than a parameter doc comment.
- Native OneOf, input defaults, root-field shorthand, non-subscription async
  list inference, and full scalar literal coercion.
- `graphql-js` SDL construction/validation in emitted-SDL builds and fixtures,
  plus removal of invalid union-member description syntax.
- Argument/directive hover and definition support from persisted source
  metadata.
- A shipped schema-merging compatibility plugin and config JSON Schema/check
  command.

The intentionally separate follow-ups are generic specialization and derived
context providers, both of which change type identity and generated runtime
lifecycle rather than merely adding schema metadata. General external-type
extensions and Federation should follow concrete integration work instead of
being hidden inside the directive core.

## Part I: directive support

### Goals

1. Define custom directives with descriptions, typed arguments, defaults,
   deprecated arguments, locations, and repeatability.
2. Apply directives to every type-system location ResGraph can author:
   `SCHEMA`, `SCALAR`, `OBJECT`, `FIELD_DEFINITION`, `ARGUMENT_DEFINITION`,
   `INTERFACE`, `UNION`, `ENUM`, `ENUM_VALUE`, `INPUT_OBJECT`, and
   `INPUT_FIELD_DEFINITION`.
3. Permit definitions that include executable locations (`QUERY`, `MUTATION`,
   `SUBSCRIPTION`, `FIELD`, fragment locations, and `VARIABLE_DEFINITION`) even
   though ResGraph does not author executable documents.
4. Validate names, locations, repeatability, required/unknown arguments, and
   constant-value coercion at build time with source-located diagnostics.
5. Preserve lexical directive order. The GraphQL specification explicitly
   allows directive order to be significant.
6. Keep generated SDL, introspection, and runtime metadata in agreement.
7. Interoperate without adapters with `@graphql-tools/utils`'s
   `getDirective(s)` functions, without giving up exact cross-directive source
   order for ResGraph-aware consumers.

### Non-goals for the first release

- A magic runtime implementation attached to the directive declaration.
  Defining `@cost`, for example, must not silently wrap resolvers.
- A general query-directive execution engine. Users can implement query
  directives with Yoga/Envelop plugins or `resolveInfo`.
- Federation behavior. Directive support is a prerequisite for Federation, not
  a complete Federation implementation.
- Immediate replacement of ResGraph's entire schema IR with `graphql-js` AST
  objects. Grats benefits from doing that, but ResGraph can take the useful
  ideas without a high-risk rewrite.

### Proposed authoring API

#### Directive definitions

A record type supplies typed directive arguments:

```rescript
/** Describes the relative execution cost of a schema element. */
@gql.directive({
  locations: ["FIELD_DEFINITION", "OBJECT"],
  repeatable: true,
})
type cost = {
  /** Number of credits consumed. */
  @gql.default(1)
  credits: int,
}
```

This produces:

```graphql
"""Describes the relative execution cost of a schema element."""
directive @cost(
  """Number of credits consumed."""
  credits: Int! = 1
) repeatable on FIELD_DEFINITION | OBJECT
```

An abstract type defines a directive without arguments:

```rescript
@gql.directive({locations: ["FIELD_DEFINITION"]})
type authenticated
```

The GraphQL directive name is the ReScript type name without capitalization.
The existing `@as("...")` convention should override it. Locations use the
spec's strings so there is no second naming scheme to learn; invalid strings
receive a diagnostic at the attribute.

Why a type rather than a function, as Grats uses:

- Records are ReScript's natural data-modeling construct and match the existing
  input-object model.
- Record-field documentation and attributes are already retained in the CMT
  summary. Function parameter defaults and parameter-level metadata are not.
- A declaration that looks callable but is never called is less natural in
  ReScript.
- The type can be inspected without inventing a runtime value or behavior.

Raw SDL is deliberately not the primary API. It would be easy to parse but
would give up ReScript-derived input types and duplicate schema knowledge.

#### Directive applications

Apply a directive with an ordered, repeatable attribute:

```rescript
@gql.annotate({name: "cost", args: {credits: 3}})
@gql.annotate({name: "authenticated"})
@gql.field
let viewer = (_: query, ~ctx: ResGraphContext.context): option<viewer> => {
  loadViewer(ctx)
}
```

The second argument is a GraphQL constant value represented with ReScript
literals: strings, integers, floats, booleans, null, arrays, records/dicts, and
nested combinations. Enum-looking strings are resolved against the declared
argument type during validation, so users do not need a second enum-literal
wrapper merely for attribute payloads.

Using a structured ReScript payload instead of
`@gql.annotate({name: "cost(credits: 3)"})` avoids introducing a second GraphQL parser
inside the native generator and follows the existing `@gql.public({...})`
style. The compiler still stores a GraphQL-shaped constant-value variant, not
arbitrary ReScript expressions. Non-constant expressions are rejected.

Multiple applications remain separate and retain source order:

```rescript
@gql.annotate({name: "tag", args: {name: "internal"}})
@gql.annotate({name: "tag", args: {name: "beta"}})
@gql.type
type user = {
  @gql.field id: ResGraph.id,
}
```

#### Schema-level directives

ResGraph currently has no source representation for a schema definition. Add
an optional marker that is only needed for a schema description, custom root
mapping, or schema directives:

```rescript
/** The public API schema. */
@gql.annotate({
  name: "link",
  args: {url: "https://specs.apollo.dev/federation/v2.11"},
})
@gql.schema
type schema
```

Initially the root mapping can remain conventional (`query`, `mutation`, and
`subscription`). The marker gives `SCHEMA` annotations a clean home and leaves
room for custom root type names later.

#### Argument-level applications and metadata

Full `ARGUMENT_DEFINITION` support and resolver defaults require source AST
information that is not present in `typedFnArg`. Before freezing syntax, add a
small spike that parses a resolver's ReScript source and correlates its
parameters with the CMT value by location. The preferred syntax is a normal
attribute attached to the labeled parameter, conceptually:

```rescript
let search = (
  _: query,
  /** Maximum number of results. */
  @gql.annotate({name: "constraint", args: {max: 100}})
  ~limit: int=20,
) => {
  // ...
}
```

The spike must confirm exactly where the ReScript parser attaches parameter
doc comments and attributes. If it cannot represent this without surprising
syntax, use a field-level fallback such as
`@gql.annotateArgument({argument: "limit", name: "constraint", args: {max: 100}})`;
do not ship both
forms speculatively.

The actual ReScript default expression should be the source of truth for
resolver arguments. `@gql.default(...)` is needed only where ReScript has no
value-level default syntax, such as directive argument records and input-object
fields.

### Internal representation

Add GraphQL constant values and directive data to
`src/ml/GenerateSchemaTypes.ml` using regular variants and records:

```ocaml
type gqlConstValue =
  | NullValue
  | IntValue of string
  | FloatValue of string
  | StringValue of string
  | BooleanValue of bool
  | EnumValue of string
  | ListValue of gqlConstValue list
  | ObjectValue of (string * gqlConstValue) list

type gqlDirectiveArgument = {
  name: string;
  value: gqlConstValue;
  loc: Location.t;
}

type gqlAppliedDirective = {
  name: string;
  arguments: gqlDirectiveArgument list;
  loc: Location.t;
  fileUri: Uri.t;
}

type gqlDirectiveLocation =
  | Query
  | Mutation
  | Subscription
  | Field
  | FragmentDefinition
  | FragmentSpread
  | InlineFragment
  | VariableDefinition
  | Schema
  | Scalar
  | Object
  | FieldDefinition
  | ArgumentDefinition
  | Interface
  | Union
  | Enum
  | EnumValue
  | InputObject
  | InputFieldDefinition

type gqlInputValueDefinition = {
  name: string;
  typ: graphqlType;
  description: string option;
  defaultValue: gqlConstValue option;
  deprecationReason: string option;
  directives: gqlAppliedDirective list;
  loc: Location.t;
  fileUri: Uri.t;
}

type gqlDirectiveDefinition = {
  name: string;
  description: string option;
  arguments: gqlInputValueDefinition list;
  repeatable: bool;
  locations: gqlDirectiveLocation list;
  loc: Location.t;
  fileUri: Uri.t;
}
```

`gqlArg` should either become `gqlInputValueDefinition` plus its
ReScript-specific `isOptionLabelled` flag, or embed one. Input-object fields can
then use the same definition metadata while keeping their property/conversion
information separate. This removes the current asymmetry where input fields
reuse the output-oriented `gqlField`, while resolver arguments contain only a
name, type, and optional-label bit.

Add ordered `directives` lists to every directable IR node and add a directive
definition table plus schema-level applied directives to `schemaState`.
Locations must be retained on nested constant values wherever practical so a
bad argument points to the literal, not merely the enclosing field.

### Extraction

`GenerateSchemaUtils.extractGqlAttribute` currently treats an unfamiliar
`gql.*` attribute as an error and is focused on finding one declaration kind.
Directive work should separate three concerns:

1. `extractDefinitionKind`: at most one of type/interface/field/etc.
2. `extractAppliedDirectives`: zero or more `@gql.annotate` values in lexical
   order.
3. Specialized metadata collectors: `@gql.implements`, authorization,
   defaults, and schema metadata.

That avoids adding more exceptions to a single `find_map` and makes repeated
metadata predictable. The same collectors must run in both the normal and
direct CMT generation paths.

For the first slice, CMT data is sufficient for types, fields, enum values,
input fields, and directive definitions. The source-AST index is needed for
function argument docs/defaults/directives. Key source data by file plus
declaration location, not only by name, so nested modules and shadowing remain
safe.

### Validation

Validate before either emitter runs:

- GraphQL names and reserved `__` prefixes.
- At least one valid location per definition and no duplicate locations.
- Definition name uniqueness, including collisions with specified directives.
- Directive argument names/types/defaults and deprecation restrictions.
- Application refers to a known built-in or custom directive.
- Application location is allowed.
- A non-repeatable directive appears no more than once at a location.
- Arguments are unique and known; all required arguments are supplied.
- Constant values coerce to the declared input types, recursively.
- OneOf values/defaults obey the current specification.
- Built-in constraints for `deprecated`, `specifiedBy`, and `oneOf`.

Grats constructs GraphQL AST and reuses `graphql-js` validation. ResGraph should
move in that direction incrementally: keep native validations for precise
ReScript locations, add a GraphQL-document exporter from the normalized IR,
and run a `graphql-js` SDL/schema validation pass in the JS CLI as a backstop.
Translate errors through schema-coordinate/source metadata already related to
the generated state file. This also catches printer drift that handwritten
validations will miss.

### Lowering built-in directives

Built-ins need native runtime behavior rather than only generic metadata:

| Directive | IR/source behavior | `graphql-js` lowering | SDL lowering |
| --- | --- | --- | --- |
| `deprecated` | Existing ReScript `@deprecated` remains preferred; generic annotation normalizes to the same semantic field | `deprecationReason` | `@deprecated(reason: ...)` |
| `specifiedBy` | Generic annotation on a custom scalar populates the existing dormant `specifiedByUrl` slot | `specifiedByURL` on `GraphQLScalarType` | `@specifiedBy(url: ...)` |
| `oneOf` | Inferred from `@gql.inputUnion`; no redundant user annotation | `isOneOf: true` on `GraphQLInputObjectType` | `@oneOf` |

Do not store these twice as independent semantic fields and generic extensions.
Normalize at extraction/validation, then let each emitter choose its native
representation. Custom directives use the generic path.

### Generated runtime representation

Add typed bindings for `GraphQLDirective`, `DirectiveLocation`,
`specifiedDirectives`, directive argument configs, `specifiedByURL`, `isOneOf`,
and `extensions` on every directable `graphql-js` config.

Custom definitions must be passed to the schema without dropping built-ins:

```rescript
GraphQLSchemaType.make({
  query: get_Query(),
  directives: [...specifiedDirectives, directive_cost],
  types: [...],
})
```

Applied custom directives should expose two projections derived from the same
ordered IR:

```js
extensions: {
  directives: {
    cost: [{credits: 3}],
    tag: [{name: "beta"}],
  },
  resgraph: {
    appliedDirectives: [
      {name: "cost", args: {credits: 3}},
      {name: "tag", args: {name: "beta"}},
    ],
  },
}
```

GraphQL Tools reads the directive-name map in `extensions.directives` by
default, so existing schema-transform packages work without a ResGraph-specific
path option. The map preserves occurrence order for one repeatable directive,
but cannot represent exact global order when names interleave, for example
`@tag @cost @tag`. The namespaced ordered list closes that loss. These are
generated views, never two independently mutable sources of truth.

In the longer term, a complete `astNode`/`extensionASTNodes` projection can
offer another standards-shaped, order-preserving view. Do not emit partial AST
nodes just for directives: consumers reasonably expect those nodes to contain
valid kinds, names, fields, and locations.

New public bindings should use concrete ReScript records rather than adding
more `{..}`/object-creator-style APIs. Constant values should remain the
existing safe JSON-style variants at the public boundary.

### SDL emission

Both emitters must consume the same IR. Add shared printers for constant values,
input-value definitions, directive definitions, and ordered applications.
Centralize GraphQL string escaping rather than interpolating raw descriptions
or deprecation reasons.

Do not switch the SDL dump to `graphql-js`'s `printSchema` and assume applied
directives survive: `GraphQLSchema` has no first-class applied-directive model.
Either keep the IR-based SDL printer or print a generated GraphQL AST/document.
The latter is the best long-term direction because it also enables standard
validation and escaping.

### Implementation slices

1. **Runtime/spec baseline**
   - Declare `graphql` as a peer dependency and choose a tested range that
     includes native OneOf support.
   - Test the floor and current `graphql@16` (currently 16.14).
   - Replace `extensions.oneOf` with `isOneOf`, remove the obsolete Envelop
     requirement, update docs, and add introspection/coercion tests.

2. **Input-value and constant-value IR**
   - Introduce `gqlConstValue` and enriched input-value definitions.
   - Add parsing/printing/runtime-codegen for constant literals.
   - Use the same foundation for `@gql.default`.

3. **Definitions and applications**
   - Add `@gql.directive` and `@gql.annotate`; add optional `@gql.schema` in a
     follow-up.
   - Collect directives on all CMT-visible type-system locations.
   - Add definition/application validation.

4. **Executable schema and SDL parity**
   - Extend `ResGraph__GraphQLJs.res` bindings.
   - Emit definitions, native built-ins, the standard directive map, and the
     ordered ResGraph projection.
   - Emit the same information in SDL.
   - Add a GraphQL Tools interoperability test.

5. **Resolver arguments and defaults**
   - Complete the source-AST correlation spike.
   - Add resolver argument descriptions, defaults, deprecations, and directive
     applications.
   - Extend hover, state/find-definition data, and diagnostics to argument
     coordinates.

6. **Tooling and documentation**
   - Completion for directive names, locations, and known argument names.
   - Hover/definition for directive declarations and applications.
   - Cache version/input updates, named-schema and multi-schema coverage.
   - A guide that distinguishes declaration, metadata, and runtime behavior.

### Directive acceptance criteria

- Generated SDL parses and validates with the supported `graphql-js` floor and
  latest v16.
- Introspection contains specified plus custom directives, including defaults,
  deprecated arguments, locations, and repeatability.
- Every supported type-system application has the same names and coerced
  arguments in SDL and via `getDirectives(schema, node)`.
- SDL and `extensions.resgraph.appliedDirectives` preserve exact source order;
  `extensions.directives` preserves occurrence order within each name.
- Invalid definition, location, duplication, missing argument, extra argument,
  and nested value cases produce source-located build diagnostics.
- `@deprecated`, `@specifiedBy`, and `@oneOf` work through native
  `graphql-js` properties and introspection.
- Named schemas, include/exclude membership, cache invalidation, authorization,
  interface inheritance, and generated ReScript compilation remain covered.

## Part II: capability map

### What ResGraph already does well

ResGraph already covers the core implementation-first server shape:

- Query, mutation, and real AsyncIterator/AsyncIterable subscriptions.
- Object, input object, interface (including interface inheritance), union,
  enum, custom scalar, list, nullable, and OneOf-shaped input variants.
- Property and function resolvers with typed context and `resolveInfo`
  injection.
- Descriptions on most schema elements and deprecation on output/input fields
  and enum values.
- Direct `graphql-js` schema codegen without runtime SDL parsing.
- Relay node/connection helpers and DataLoader bindings.
- Multi-schema projects with include/exclude ownership.
- Required authorization coverage with source-aware manifests/baselines.
- Dedicated LSP/VS Code workflow, hovers/completion, schema-coordinate
  definition lookup, stable output, and incremental generation cache.

Multi-schema generation and required authorization are meaningful ResGraph
advantages and should remain central; they are not Grats gaps to copy.

### Spec and schema-fidelity gaps

| Capability | ResGraph today | Recommended action | Priority |
| --- | --- | --- | --- |
| Custom directive definitions/applications | Implemented across every type-system location, including schema and source-correlated resolver arguments | Add broader named-schema fixtures and code actions | Delivered/P2 tooling |
| Runtime access to applied directives | Standard GraphQL Tools map and exact ordered ResGraph projection implemented | Add transformation recipes and expand multi-schema coverage | Delivered/P1 docs |
| Argument defaults | Resolver source defaults are correlated, validated, emitted, and source-addressable in tooling | Add more nested-constant fixtures | Delivered |
| Argument descriptions/deprecations/directives | Parameter attributes populate enriched argument IR; `Type.field.argument` hover/definition resolves source metadata | Add targeted completion/code actions | Delivered/P2 tooling |
| Input-field defaults | `@gql.default(const)` is validated and emitted in SDL/runtime input-field configs | Expand source tooling and nested-cycle diagnostics with the validation backstop | Delivered/P1 tooling |
| `@specifiedBy` | Native `@specifiedBy("...")` emits SDL and `specifiedByURL` | Consider normalization through the generic annotation path | Delivered/P2 |
| Standard OneOf | Native `isOneOf`, SDL, introspection, and coercion on `graphql@^16.11 || ^17`; no plugin required | Keep floor/latest compatibility coverage current | Delivered |
| Schema definition metadata | Optional `@gql.schema` marker emits description, directives, and typed root mappings in SDL/runtime config | Add Federation-oriented examples in a separate layer | Delivered |
| Type-system extensions | Field functions compose object/interface fields inside one ResGraph schema, but no general schema/scalar/union/enum/input extension model or external target | Model explicit/external extensions only when driven by migration/Federation use cases | P1/P2 |
| Full schema validation | Targeted native checks are backed by `graphql-js` SDL construction and validation in fixtures and CLI builds that emit SDL | Preserve validation for non-dumped SDL and translate SDL coordinates to author sources | P0 in progress |
| SDL fidelity | Generated fixtures are parsed and constructed by `graphql-js`; unsupported union-member descriptions are no longer emitted | Centralize escaping and expand fixture coverage | P0 in progress |
| Scalar coercion surface | Module convention supports `parseValue`, `parseLiteral`, `serialize`, and `specifiedByURL` with typed value-node conversion | Add an explicit scalar-config escape hatch only when a concrete integration needs it | Delivered/P2 escape hatch |
| Custom root type names | `@gql.schema` maps query, mutation, and subscription to any authored object type | Add multi-schema fixtures and editor completion for mapping names | Delivered/P1 tooling |

### Gaps relative to Grats

| Grats capability | ResGraph status | What to do | Priority |
| --- | --- | --- | --- |
| Directive definitions and generic annotations | Definitions and applications cover all type-system locations with standard and ordered runtime projections | Finish tooling and transformation recipes | Delivered/P1 tooling |
| Resolver argument defaults and metadata | Defaults, descriptions, deprecations, and directives implemented through source-AST correlation | Extend state/LSP data and code actions | Delivered/P1 tooling |
| Generic object/interface/union/input materialization | Annotated generic named types are not deliberately monomorphized into distinct GraphQL types | Design deterministic specialization names and cycle-safe memoized materialization; start with connections/results | P1 |
| Derived context values, including async | One configured context type only | Add `@gql.context` provider functions, dependency graph validation, cycle detection, and per-request memoization semantics | P1 |
| Non-subscription `AsyncIterable<T>` for `@stream` | Resolver return values map to `[T]` while retaining the async iterable for `graphql-js` execution | Add an incremental-delivery server integration fixture | Delivered/P1 integration |
| Root-field shorthand | `@gql.query`, `@gql.mutation`, and `@gql.subscription` synthesize conventional roots and omit the source argument | Extend editor snippets/completion and named-schema fixtures | Delivered/P1 tooling |
| Nullable-by-default and semantic non-null mode | Nullability maps directly from ReScript `option`/nullable types | Do not copy blindly: ReScript is more sound than TypeScript. Revisit after generic directives, as an opt-in policy with runtime checks | P2/experimental |
| Full scalar schema config (`serialize`, `parseValue`, `parseLiteral`) | Module-name convention supports all three hooks and typed AST-to-literal conversion | Consider an explicit config escape hatch; retain zero-config inference | Delivered/P2 escape hatch |
| Incremental schema migration | `mergeSchemas` integration now uses a shipped, execution-tested compatibility plugin; shared types still need duplication | Evaluate external type placeholders/resolver-map output from concrete migrations | Delivered/P1 external types |
| Resolver-map output | No equivalent | Consider after migration requirements are concrete; executable schema remains the default | P2 |
| Emitted metadata | Persisted state includes directives, enriched arguments, schema metadata, and source coordinates; hover/definition consumes argument/directive locations | Add a stable external JSON projection only for a demonstrated consumer | Delivered/P2 projection |
| Generated client enum module | ReScript variants already are runtime/client-usable in the authoring language | No direct port needed; document client-codegen integration instead | Not needed |
| `--fix` and code actions | Diagnostics/completion/hover exist, but no general fix workflow | Add focused fixes for attribute spelling, obsolete OneOf setup, and safe migrations after syntax stabilizes | P2 |
| Schema headers/config schema | Shipped JSON Schema covers legacy/named configs and `resgraph check` validates semantic/path constraints | Consider cosmetic generated-header options only with a concrete use case | Delivered/P2 headers |

### Broader GraphQL ecosystem opportunities

These are not all core compiler obligations. The recommended response is often
an interoperable hook or documented integration rather than built-in policy.

| Area | Missing piece | Recommended response | Priority |
| --- | --- | --- | --- |
| Apollo Federation/subgraphs | No schema `@link`, federation directives, entity union/reference resolver, or subgraph SDL workflow | Directives/schema annotations first; then a separate `ResGraphFederation` layer with entity types/resolvers and conformance fixtures | P1/P2 |
| Schema transforms and policy directives | Standard GraphQL Tools map plus ordered ResGraph projection are available to transforms | Add cost/cache/formatting recipes without implicit compiler behavior | Delivered/P1 docs |
| Incremental delivery | Subscriptions and non-subscription AsyncIterable list inference are supported; `@defer`/`@stream` transport remains server-driven | Add Yoga incremental-delivery integration tests; avoid owning transport protocol | Delivered/P1 integration |
| Persisted operations, complexity limits, tracing, response caching | Not compiler/type-system features | Improve Yoga/Envelop bindings and recipes; directives can carry static cost/cache metadata | P2/docs |
| Scalar ecosystem | Specification URLs and all three coercion hooks are supported | Add scalar-registry examples and explicit configs only when needed | Delivered/P2 docs |
| External schemas/stitching | Shipped compatibility glue handles merged resolver sources; duplicate definitions remain necessary | Test a full `mergeSchemas` fixture, then evaluate external type placeholders/resolver-map output | Delivered/P1 external types |
| Emerging nullability | No semantic-null metadata or nullability-assertion experiments | Build on generic directives; keep opt-in and track the active RFC rather than hard-coding draft syntax | Experimental |
| Client/tooling interoperability | State is ResGraph-specific and SDL can omit metadata | Valid SDL/AST, stable schema coordinates, directive extensions, and documented GraphQL Code Generator/Relay flows | P1 |

### Prioritized roadmap

#### P0: make the schema representation current and trustworthy

1. ~~Declare/test the `graphql` peer range and modernize OneOf.~~ Delivered on
   the stacked roadmap branch.
2. ~~Complete the constant/input-value foundation and resolver metadata.~~
3. ~~Finish schema/argument directive locations.~~
4. ~~Add SDL/executable parity tests and a `graphql-js` backstop.~~ Delivered
   for fixtures and CLI builds that emit SDL; non-dumped SDL/source remapping is
   the remaining hardening slice.
5. ~~Remove invalid union-member description emission.~~ Continue centralizing
   escaping as emitters are touched.

This group should land before building Federation or more directive-based
features. Otherwise each new feature adds another one-off metadata path.

#### P1: close the practical Grats gaps

1. ~~Argument/input defaults, descriptions, deprecations, annotations, and
   source tooling.~~
2. ~~Root-field shorthand.~~
3. ~~Non-subscription AsyncIterable/list support for `@stream`.~~
4. Generic type specialization for high-value patterns.
5. Derived context providers with explicit memoization behavior.
6. ~~Full scalar coercion hooks.~~ Explicit config remains a P2 escape hatch.
7. ~~Ship and test schema-merging compatibility.~~ External type placeholders
   remain integration-driven.
8. ~~Directive-aware tooling/state metadata.~~

#### P2: ecosystem breadth and polish

1. Federation as an optional layer.
2. Resolver-map output if migration users need it.
3. Opt-in semantic nullability experiments.
4. ~~CLI `check` and config JSON Schema.~~ Safe code actions remain.
5. ~~Custom root names.~~ Broader explicit type extensions require a real use
   case.

### Specifications for the next focused PRs

#### Generic specialization

Treat an annotated generic declaration as a template, not as a schema type by
itself. Materialize it only when a concrete instantiation is reachable from a
root or another materialized type. The specialization key must contain the
declaration's stable source identity plus recursively normalized concrete type
arguments. Insert a placeholder in the memo table before expanding fields so
recursive types terminate. Generate a deterministic GraphQL name from the base
name and argument names, reject collisions with an actionable `@as` override,
and persist the specialization-to-source mapping for hover/definition. Start
with object and input-object records; add interfaces/unions only after variance
and resolver dispatch have dedicated fixtures.

#### Derived context providers

Use `@gql.context` on a function whose return type is the provided context
identity. Labelled parameters may request the configured base context or other
provided contexts. Build and validate the provider dependency DAG before field
argument inference, reject duplicate providers and cycles at their source
locations, and make asyncness part of the provider IR. Generated resolvers must
memoize each value or in-flight promise once per request context so sibling
fields share work and failures consistently. Provider calls should remain
explicit generated code; no process-global value cache and no hidden server
plugin.

#### External schemas and Federation

Keep the shipped compatibility plugin as the migration baseline. Add external
type placeholders only with a real merged-schema fixture that proves ownership,
resolver, and validation semantics. Build Federation as a separate optional
layer over schema directives (`@link`, `@key`, and friends), entity reference
resolution, and subgraph SDL conformance; do not teach the generic directive
engine Apollo-specific runtime behavior.

### What not to copy from Grats verbatim

- Do not use a function declaration for a directive merely because Grats does;
  a record/abstract type fits ReScript and the available metadata better.
- Do not put directive data only in a private namespace. Emit the GraphQL Tools
  map for interoperability and keep only the extra cross-name ordering view
  under `extensions.resgraph`.
- Do not make directive declarations execute implicitly. Runtime behavior must
  be an explicit transform/plugin so ordering and server lifecycle are visible.
- Do not adopt nullable-by-default as a default merely to match Grats. Its
  trade-off is strongly influenced by TypeScript's soundness and common
  GraphQL error policy.
- Do not rewrite the whole compiler around GraphQL AST in one step. First add a
  GraphQL-shaped boundary and validation/export path, then migrate internals
  only where it removes real duplication.

## Source trail

Local ResGraph areas audited:

- `src/ml/GenerateSchemaTypes.ml` — schema IR; minimal `gqlArg`; dormant
  `specifiedByUrl`.
- `src/ml/GenerateSchemaUtils.ml` — GraphQL attribute extraction and schema
  registration; hard-coded `specifiedByUrl = None`.
- `src/ml/GenerateSchema.ml` — CMT traversal, type inference, resolver argument
  mapping, and subscription-only async iterable inference.
- `src/ml/GenerateSchemaValidation.ml` — native schema validation.
- `src/ml/GenerateSchemaTypePrinters.ml` — executable `graphql-js` codegen and
  hard-coded `extensions.oneOf`.
- `src/ml/GenerateSchemaSDL.ml` — separate handwritten SDL emitter.
- `src/res/ResGraph__GraphQLJs.res` — runtime constructor/config bindings.
- `tests/src/AppCustomScalars.res` — currently ignored `@specifiedBy` fixture.
- `TODO.md`, docs, generated fixtures, multi-schema tests, and authorization
  fixtures.

External primary references:

- GraphQL September 2025 specification:
  <https://spec.graphql.org/September2025/>
- Accepted OneOf RFC:
  <https://rfcs.graphql.org/rfcs/825/>
- `graphql-js` releases:
  <https://github.com/graphql/graphql-js/releases>
- Grats directive definitions:
  <https://grats.capt.dev/docs/docblock-tags/directive-definitions>
- Grats directive annotations and runtime representation:
  <https://grats.capt.dev/docs/docblock-tags/directive-annotations>
- Grats changelog and configuration:
  <https://grats.capt.dev/docs/changelog/>
  <https://grats.capt.dev/docs/getting-started/configuration>
- Grats generics, derived context, and stream support:
  <https://grats.capt.dev/docs/guides/generics/>
  <https://grats.capt.dev/docs/docblock-tags/context/>
  <https://grats.capt.dev/docs/guides/stream/>
- GraphQL Tools schema directive convention:
  <https://the-guild.dev/graphql/tools/docs/schema-directives>
- GraphQL Tools implementation of directive-extension lookup:
  <https://github.com/ardatan/graphql-tools/blob/master/packages/utils/src/getDirectiveExtensions.ts>
