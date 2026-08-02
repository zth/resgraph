# Required authorization coverage

ResGraph can require every application-defined GraphQL output field to have an explicit authorization disposition. Enable it in `resgraph.json`:

```json
{
  "src": "./src",
  "outputFolder": "./src/schema/__generated__",
  "authorization": {
    "mode": "required",
    "onForbidden": "Security.onForbidden",
    "manifestPath": "./src/schema/__generated__/authorization-manifest.json",
    "baselinePath": "./src/schema/__generated__/authorization-baseline.json"
  }
}
```

`onForbidden`, `manifestPath`, and `baselinePath` are optional. Manifest and baseline paths resolve relative to `resgraph.json`. During generation, the manifest has a `generationFailed` status and empty fields; it changes to `success` only after every schema artifact is written. ResGraph refuses to overwrite an existing file that lacks its generated-manifest marker. Introspection fields are outside this check. Subscription fields require an `unsupportedSubscription` baseline entry until their authorization execution semantics are supported.

With [multiple named schemas](multiple-schemas), put `authorization` inside each schema that requires coverage. Use distinct manifest and baseline paths for every schema. `resgraph authorization baseline <schema>` refreshes a named schema; omitting the name uses `defaultSchema`.

## Incremental adoption with a baseline

A baseline lets an existing codebase enable required mode immediately without adding suppression annotations to application source. Set `baselinePath`, compile the project, and snapshot the current gaps:

```sh
resgraph authorization baseline
```

The generated JSON records exact field coordinates and the kind of gap at each coordinate. Commit it and run ordinary `resgraph build` commands in CI. A required build then:

- Allows only the gaps already present in the baseline.
- Rejects every new uncovered field or new mutation/subscription gap.
- Rejects stale entries after a field is covered or removed, so the baseline must shrink as adoption progresses.

Whenever `baselinePath` is configured, each successful compilation prints a warning that fields listed in the baseline are outside required authorization coverage.

Add `@gql.authorize`, `@gql.public`, or an appropriate resolver outcome one field at a time, then remove the corresponding stale entry. This makes the migration a ratchet: normal builds cannot silently grow the exception set.

Run `resgraph authorization baseline` again only when intentionally refreshing the snapshot. The command can add current gaps, so review its diff like any other security-policy change. It refuses to overwrite files that are not marked as ResGraph-generated and keeps entries sorted for stable diffs.

The audit manifest reports an allowed legacy gap with the `baseline` disposition. Baseline gap kinds distinguish uncovered fields, outcome-only mutations that lack a pre-resolver policy, and subscriptions whose authorization execution semantics are not yet supported.

## Authorization policies

Policies are module-qualified functions attached with repeatable `@gql.authorize` annotations:

```rescript
@gql.authorize(UserSecurity.canRead)
@gql.type
type user = {
  @gql.field id: string,
  emailAddress: string,
}

@gql.authorize(UserSecurity.canReadEmail)
@gql.field
let email = (user: user, ~includeUnverified: bool): string => user.emailAddress

type reason = IncludeUnverifiedDenied

let canReadEmail = (
  user: user,
  ~args,
  ~ctx: ResGraphContext.context,
  ~info: ResGraph.resolveInfo,
) => {
  let includeUnverified = args["includeUnverified"]
  let _ = (ctx, info)
  // Return ResGraph.Authorization.Allowed() or ResGraph.Authorization.Forbidden(reason).
  if includeUnverified {
    ResGraph.Authorization.Forbidden(IncludeUnverifiedDenied)
  } else {
    ResGraph.Authorization.Allowed()
  }
}
```

A policy must have:

- The owning object or interface as its first unlabelled argument.
- A mandatory `~args` polymorphic object (`{}` for fields without arguments).
- Optional `~ctx` and `~info` injections, which are passed only when declared.
- An `outcome<unit, 'reason>` return value, optionally wrapped in a promise.

Type, interface, field, and resolver policies compose additively. Interface field public dispositions and interface resolver outcomes also propagate to concrete implementations. Policies execute in declaration order with AND semantics and stop at the first `Forbidden`. Converted field arguments are constructed once and shared by every policy and the resolver. Async code is generated only when a policy or outcome is promise-backed.

By default, a policy on a field returning an object does not authorize that
object's child fields. Each output field needs its own disposition.

### Authorization by an ancestor

Connection wrappers, mutation payloads, and calculated result objects often
have no authorization identity independent of the field that produced them.
After reviewing that relationship, mark their fields as authorized by an
ancestor:

```rescript
@gql.authorize.byAncestor({
  reason: "Connection fields expose only the authorized campaign result",
})
@gql.type
type campaignConnection = {
  @gql.field
  edges: array<campaignEdge>,
}

@gql.authorize(CampaignSecurity.canList)
@gql.field
let campaigns = async (
  _: query,
  ~organizationId: string,
  ~ctx: ResGraphContext.context,
): campaignConnection => {
  await ctx.dataLoaders.campaigns.load(~organizationId)
}
```

On a concrete object type, `@gql.authorize.byAncestor` is a default for each
immediate GraphQL field, including separately declared resolver fields. It is
not recursively copied to returned object types: `campaignEdge` must declare
its own disposition. The annotation can also be placed directly on one field.
A direct field policy, public declaration, or resolver outcome overrides a type
default. A field-level `byAncestor` annotation cannot be combined with one of
those direct dispositions.

ResGraph statically proves that every root-to-field path crosses an ordinary
`@gql.authorize(...)` policy or an allowed resolver outcome before the annotated
field. A public field is not an authorization boundary. If the same object type
is reachable through an unprotected path, generation fails at the
`byAncestor` annotation. Lists and nullability are transparent to the proof;
unions and interface return types fan out to their concrete object types.
`byAncestor` is currently supported on concrete object types and fields, not
on interfaces or subscription paths. Paths from subscription roots are treated
as unprotected even when the subscription field declares a policy, because
ResGraph cannot yet enforce that policy for each delivered event. Shared return
types therefore cannot hide an unsafe subscription path.

The annotation generates no runtime check. Upstream policies run only where
they are declared, so expensive authorization is not reevaluated for structural
child fields. Mutation root fields still require a pre-resolver policy before a
payload type may use `byAncestor`; a mutation's post-resolver outcome does not
serve as that ancestor boundary.

## Resolver outcomes

A resolver can perform a post-resolution exposure check by returning an outcome:

```rescript
@gql.field
let email = (
  user: user,
  ~ctx: ResGraphContext.context,
): ResGraph.Authorization.outcome<string, Security.reason> =>
  if Security.canReadEmail(user, ~ctx) {
    ResGraph.Authorization.Allowed(user.email)
  } else {
    ResGraph.Authorization.Forbidden(CannotReadEmail)
  }
```

ResGraph uses the `Allowed` payload as the GraphQL return type and unwraps it during execution. A resolver outcome alone is not sufficient for a non-public mutation because it runs after resolver side effects; mutations require a pre-resolver policy.

## Public fields

Use `@gql.public` for intentionally public fields. A reviewable reason with at least three non-whitespace characters is required:

```rescript
@gql.public({reason: "Contains no tenant or user data"})
@gql.field
let health = (_: query) => "ok"
```

Public coverage cannot be combined with a policy or resolver outcome.

## Forbidden responses

By default, denial raises a generic GraphQL error with message `Forbidden` and `extensions.code = "FORBIDDEN"`. The typed policy reason is not exposed to clients.

A configured handler receives the reason plus GraphQL context and resolve info, and returns a client-safe `ResGraph.Authorization.error` value that generated code raises:

```rescript
let onForbidden = (
  reason,
  ~ctx: ResGraphContext.context,
  ~info: ResGraph.resolveInfo,
) => {
  SecurityAudit.record(reason, ctx)
  ResGraph.Authorization.makeError(~message="Not authorized", ~code="FORBIDDEN")
}
```

## Audit manifest and boundaries

`manifestPath` emits stable, sorted JSON with every concrete field, its disposition, policy order and provenance, public or `byAncestor` review reason, source locations, resolver-outcome metadata, and mutation status. For an `authorizedByAncestor` field, `ancestorBoundaries` lists the nearest possible policy or resolver-outcome boundaries across its protected paths. Only policies declared on the selected runtime path execute. Commit the manifest when authorization posture should be reviewed through diffs.

Fields on inferred union payload objects use the `synthetic` disposition. They
have no annotation surface and are reachable only after their parent resolver
field has passed its own required authorization disposition.

Required authorization coverage is a structural guarantee that checks are declared and generated. It does not prove policy correctness, authentication setup, or collection filtering. Authorization on a list gates access to the list field but does not scope its items, lengths, ordering, cursors, or existence. Filter collections in the data/domain layer, and keep authorization caches request-scoped unless they include every security-relevant identity, tenant, action, argument, and invalidation dimension.
