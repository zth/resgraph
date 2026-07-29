# Required authorization coverage

Status: implemented

## Goal

Add an opt-in required-authorization-coverage mode where ResGraph refuses to
build a schema if any application-defined output field can execute without an
explicit authorization disposition.

Every executable field must be covered by one of:

1. One or more `@gql.authorize(...)` functions, declared on the field, its
   resolver, or its parent type.
2. A resolver whose return type is an authorization outcome.
3. `@gql.public({reason: "..."})`.

Input fields do not require authorization. Built-in GraphQL introspection is
outside the initial guarantee and must be documented explicitly.

For a non-public mutation, a resolver outcome alone is not sufficient coverage.
At least one applicable `@gql.authorize(...)` function must run before the
resolver so an unauthorized mutation cannot perform side effects before
returning `Forbidden`. `@gql.public({reason})` remains an explicit way to
declare a genuinely public mutation.

## Guarantee and boundaries

Required authorization coverage is a structural enforcement guarantee. It
proves that:

- Every concrete executable application output field has an explicit
  authorization disposition.
- Generated execution invokes the complete effective policy plan.
- Referenced policy functions have compiler-checked source, arguments,
  injections, and outcome types.
- Authorization of an object-returning field does not implicitly authorize its
  child fields.

It does not prove that:

- An authorization function implements the correct business rule.
- A public reason is factually correct.
- A returned collection was filtered to the correct set of objects.
- Authentication or GraphQL context construction is correct.
- Non-GraphQL entry points enforce the same policy.
- A resolver-outcome check happened before arbitrary work performed inside the
  resolver.

Documentation and diagnostics must call this feature "required authorization
coverage", not a proof that application authorization is correct.

## API

Enable enforcement in `resgraph.json`:

```json
{
  "authorization": {
    "mode": "required",
    "onForbidden": "Security.onForbidden",
    "manifestPath": "generated/resgraph-authorization.json"
  }
}
```

`manifestPath` is optional and is resolved relative to `resgraph.json`. When
present, ResGraph emits a deterministic authorization manifest suitable for
review and CI diffing.

Authorization functions are repeatable and compose. All applicable functions
must allow access.

```rescript
@gql.authorize(UserSecurity.canRead)
@gql.type
type user = {
  @gql.field
  id: string,

  @gql.authorize(UserSecurity.canReadEmail)
  @gql.field
  email: string,

  @gql.public({reason: "The display name is part of the public profile"})
  @gql.field
  displayName: string,
}
```

An authorization function has a stable shape:

```rescript
let canReadEmail = (
  user: user,
  ~args,
  ~ctx: ResGraphContext.context,
): Authorization.outcome<unit, reason> => {
  // `args` is always present. This field has no arguments, so it is `{}`.
}
```

- The first unlabelled argument is the typed source object.
- `~args` is mandatory and is always a ReScript polymorphic object.
- `~ctx`, `~info`, and future injections are ordinary optional labelled
  arguments. ResGraph passes only the injections requested by the function.
- The function returns `outcome<unit, 'reason>` or a promise of that outcome.
- Multiple applicable functions run in deterministic order and short-circuit
  on the first forbidden result.

Authorization functions should normally be thin adapters over shared
application or domain authorization services. Required mode guarantees that
the adapter is called at the GraphQL boundary; it should not make GraphQL the
only entry point that knows the business policy.

For a field with GraphQL arguments:

```rescript
let canReadDocument = (
  organization: organization,
  ~args,
  ~ctx: ResGraphContext.context,
) => {
  ctx.documentAccess.load((organization.id, args["documentId"]))
}
```

`args` contains every field argument, keyed by its exposed GraphQL name and
converted to the same ReScript representation supplied to the resolver.
Optional arguments, enums, input objects, input unions, and custom scalars must
therefore behave identically in authorization functions and resolvers.

If no annotation applies, a resolver can satisfy required mode by returning an
outcome:

```rescript
@gql.field
let email = (
  user: user,
  ~ctx: ResGraphContext.context,
): Authorization.outcome<string, Security.reason> => {
  if Security.canReadEmail(user, ~ctx) {
    Allowed(user.email)
  } else {
    Forbidden(CannotReadEmail)
  }
}
```

ResGraph uses the allowed payload (`string`) as the GraphQL field type and
unwraps the outcome at runtime. A resolver may still return an outcome when
annotations apply; in that case all pre-resolver functions run first and the
resolver outcome is checked afterward.

Resolver outcomes are post-resolver exposure checks. They are useful when a
decision needs the resolved value, especially on root query fields whose source
object is only the GraphQL root. They do not by themselves guarantee that work
or side effects inside the resolver were authorized before execution.

`@gql.public` is an explicit authorization disposition, not an unchecked
default. Its structured payload must contain a reason with at least three
non-whitespace characters:

```rescript
@gql.public({reason: "Contains no tenant or user data"})
```

Using `@gql.public` together with `@gql.authorize` on the same effective field
is an error.

## Coverage and composition

For each concrete executable field, build an authorization plan in this order:

1. Parent-type authorization functions.
2. Field/property authorization functions.
3. Resolver authorization functions.
4. Resolver outcome unwrapping.

Functions add restrictions; a more specific function never replaces an
inherited function.

A successful check on a field returning an object does not authorize that
object's child fields. Each child field needs its own plan. This is required to
prevent authorization of `Query.user` from implicitly exposing all `User`
fields.

Interface declarations are not themselves executed. Their authorization
metadata must be propagated to each concrete implementing field, where runtime
enforcement occurs. Overrides add to inherited interface policies.

Required mode rejects subscriptions until both subscription creation and event
delivery have defined enforcement semantics. This is safer than claiming
incomplete coverage.

### Collections

Authorization coverage on a list or connection field gates access to that
field; it does not prove that every returned item was correctly scoped. Item
existence, list length, ordering, and pagination cursors may reveal information
even when every child field has its own policy.

In v1, applications remain responsible for filtering collections in their data
or domain layer. Documentation must distinguish field authorization from
collection scoping. A future `@gql.scope(...)` facility may return a filtered
collection or contribute a query plan, but it is not part of the initial
coverage guarantee.

## Generated code

Construct converted arguments once per field resolution and reuse them for all
authorization functions and the resolver:

```rescript
let args = {
  "documentId": convertedDocumentId,
  "includeArchived": convertedIncludeArchived,
}

switch await OrganizationSecurity.canRead(~args, ~ctx, source) {
| Allowed() =>
  switch await DocumentSecurity.canRead(~args, ~ctx, source) {
  | Allowed() =>
    DocumentResolver.document(
      source,
      ~documentId=args["documentId"],
      ?includeArchived=args["includeArchived"],
    )
  | Forbidden(reason) =>
    ResGraph.Authorization.raiseError(Security.onForbidden(reason, ~ctx, ~info))
  }
| Forbidden(reason) =>
    ResGraph.Authorization.raiseError(Security.onForbidden(reason, ~ctx, ~info))
}
```

The actual generated call must preserve ReScript's argument order and syntax.
The example illustrates the execution pipeline.

For fields without arguments, pass `~args={}`. All applicable authorization
functions share the same object. Do not add runtime reflection, string-based
function lookup, or automatic authorization caching.

Keep synchronous authorization functions synchronous. Generate asynchronous
code only when a function or resolver outcome is promise-backed.

The application owns authorization performance and caching, normally through
request-scoped DataLoaders or services available on `ctx`. ResGraph only
constructs the plan, performs conversions once, calls functions directly, and
short-circuits on denial.

Authorization caching is security-sensitive. Documentation should recommend
that application cache keys include the actor/session, action or policy,
resource identity, relevant field arguments, and tenant or security-version
information when applicable. Caches should be request-scoped unless the
application has an explicit invalidation and consistency model.

## Forbidden behavior

A denied policy or resolver outcome produces a GraphQL error with the stable
default extension code `FORBIDDEN` and follows normal GraphQL null propagation.
Required mode does not initially support silently converting denial to `null`
or an empty list.

The raw typed `Forbidden(reason)` payload is internal. ResGraph must not expose
it to clients by default. The configured `onForbidden` handler may log, map, or
replace the client-safe error, but the default behavior remains a generic
forbidden error. A configured handler receives `(reason, ~ctx, ~info)` and
returns a client-safe `ResGraph.Authorization.error` value, which generated code
always raises. Normal ReScript type checking
verifies that the handler accepts every effective reason type.

## Authorization manifest

When `manifestPath` is configured, the compiler emits a deterministic manifest
containing, for every concrete executable field:

- The GraphQL coordinate.
- Its final disposition: policy plan, resolver outcome, or public.
- Every effective policy function in execution order and its declaration
  source location.
- Whether a resolver outcome is also checked.
- The source and reason for a public disposition.
- Metadata showing inherited and interface-propagated policies.

The manifest contains no runtime identities, arguments, policy results, or
secrets. Its purpose is to make authorization posture and public-field changes
visible in code review and CI.

## Compiler representation

Extend the schema model with:

```text
authorizationFunction
  - source location
  - module/function path
  - sync or async
  - required args object row
  - requested injections

declaredAuthorizationMetadata
  - locally declared function list
  - resolver returns outcome
  - optional public reason and source location

effectiveAuthorizationPlan
  - ordered effective function list with provenance
  - check resolver outcome
  - optional public reason and source location

gqlField
  - existing field data
  - declared authorization metadata
  - effective authorization plan
```

Keep declared metadata separate from the effective plan so inheritance,
interface propagation, conflicts, and diagnostics can be calculated after the
full schema is known.

The authorization outcome is the ResGraph-owned regular variant
`ResGraph.Authorization.outcome`:

```rescript
type outcome<'value, 'reason> =
  | Allowed('value)
  | Forbidden('reason)
```

The configured `onForbidden` handler is called from generated code and its
returned `ResGraph.Authorization.error` is always raised.
Normal ReScript type checking therefore verifies its compatibility with every
effective policy and resolver-outcome reason type.

## Validation and diagnostics

Required mode must report source-located errors for:

- An executable output field with no authorization disposition.
- An authorization function without exactly one source argument.
- A missing mandatory `~args` argument.
- `~args` not being an object type.
- A source type that does not match the field's parent type.
- An argument required by the `args` object row that the field does not define.
- An unsupported labelled argument/injection.
- An invalid authorization return type.
- `@gql.public` with a reason shorter than three non-whitespace characters.
- Conflicting public and authorization annotations.
- A resolver outcome whose allowed payload is not a valid GraphQL output type.
- A non-public mutation covered only by a resolver outcome.
- Subscriptions while their required-mode semantics are unsupported.

Diagnostics should name the GraphQL coordinate, policy function, and missing or
invalid requirement, for example:

```text
Authorization function `DocumentSecurity.canRead` requires argument
`documentId`, but `Organization.documents` does not define it.
```

## Implemented sequence

1. **Finalize the outcome and denial contracts**
   - Choose the public module/type names.
   - Define how `Forbidden(reason)` becomes a GraphQL error.
   - Preserve the stable default `FORBIDDEN` code without exposing raw reasons.
   - Do not add a silent-null mode.
   - Decide whether v1 requires one application-wide reason type.

2. **Parse configuration and attributes**
   - Extend `cli/Utils.res` configuration parsing.
   - Pass authorization configuration to the native generator.
   - Add repeatable `gql.authorize` and singular `gql.public` extractors in
     `src/ml/GenerateSchemaUtils.ml`.
   - Parse authorization payloads as function paths and public payloads as
     structured reasons.
   - Parse the optional deterministic manifest output path.

3. **Extract authorization function types**
   - Add schema-model types in `src/ml/GenerateSchemaTypes.ml`.
   - In `src/ml/GenerateSchema.ml`, resolve each referenced function from CMT
     data.
   - Validate source, mandatory polymorphic `~args`, requested injections,
     return outcome, and sync/async shape.
   - Read the open object row to determine which GraphQL arguments the function
     requires.

4. **Build and validate effective plans**
   - Attach metadata to types, properties, and resolver functions.
   - Propagate type and interface policies to concrete fields.
   - Compose policies deterministically.
   - Unwrap resolver outcome types before GraphQL type generation.
   - Run total-coverage and conflict validation after schema processing.
   - Require a pre-resolver policy for every non-public mutation.

5. **Generate enforcement**
   - Update `src/ml/GenerateSchemaTypePrinters.ml`.
   - Generate one converted `args` object per resolution.
   - Pass it to every authorization function and reuse its fields for the
     resolver call.
   - Emit synchronous and asynchronous pipelines without runtime lookup.
   - Route forbidden results through the configured handler.

6. **Emit the authorization manifest**
   - Serialize final concrete-field plans in a stable coordinate order.
   - Include policy provenance, public reasons, and resolver-outcome metadata.
   - Add snapshot tests proving stable output and inherited-policy visibility.

7. **Tests and documentation**
   - Add positive fixtures for record properties, handwritten resolvers,
     multiple and inherited policies, arguments, injections, async policies,
     resolver outcomes, and public fields.
   - Add a diagnostic fixture for every validation rule above.
   - Add runtime tests proving denial prevents resolver execution and policies
     short-circuit in order.
   - Add a mutation fixture proving outcome-only coverage is rejected.
   - Test the stable client-safe `FORBIDDEN` error and normal null propagation.
   - Cover optional and complex argument conversion.
   - Document the exact coverage guarantee, collection-scoping boundary,
     introspection boundary, nested-field behavior, mutation rules, shared
     business-policy recommendation, and application-owned caching.

## Initial acceptance criteria

- Enabling required mode makes an existing unsecured schema fail to build.
- Every application-defined concrete output field is covered or diagnosed.
- Record-backed fields can be secured without handwritten accessor resolvers.
- Policies receive a typed source, mandatory polymorphic `~args`, and requested
  labelled injections.
- Multiple and inherited policies all execute and short-circuit on denial.
- Resolver outcomes are unwrapped without changing the emitted GraphQL type.
- A non-public mutation without a pre-resolver authorization function fails to
  build even if its resolver returns an outcome.
- Public fields require an auditable reason of at least three non-whitespace
  characters.
- A forbidden pre-resolver policy prevents the resolver, including a mutation,
  from executing.
- Denial produces a client-safe `FORBIDDEN` error without exposing the raw
  reason and without a silent-null mode.
- An optional deterministic manifest describes every effective field plan and
  public reason.
- No policy lookup or schema traversal occurs during field resolution.

## Deferred

- Arbitrary application-defined outcome wrapper types.
- Automatic caching or memoization.
- Annotation-level OR composition.
- Operation-level authorization planning.
- Collection scoping and policy-produced query plans.
- Subscription authorization.
- Introspection authorization.
- Capability-carrying authorized values.
