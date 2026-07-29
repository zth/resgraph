# Required authorization coverage

ResGraph can require every application-defined GraphQL output field to have an explicit authorization disposition. Enable it in `resgraph.json`:

```json
{
  "src": "./src",
  "outputFolder": "./src/schema/__generated__",
  "authorization": {
    "mode": "required",
    "onForbidden": "Security.onForbidden",
    "manifestPath": "./src/schema/__generated__/authorization-manifest.json"
  }
}
```

`onForbidden` and `manifestPath` are optional. Manifest paths resolve relative to `resgraph.json`. During generation, the manifest has a `generationFailed` status and empty fields; it changes to `success` only after every schema artifact is written. ResGraph refuses to overwrite an existing file that lacks its generated-manifest marker. Introspection fields are outside this check, and required mode currently rejects subscriptions.

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

A policy on a field returning an object does not authorize that object's child fields. Each output field needs its own disposition.

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

`manifestPath` emits stable, sorted JSON with every concrete field, its disposition, policy order and provenance, public reason, source locations, resolver-outcome metadata, and mutation status. Commit it when authorization posture should be reviewed through diffs.

Fields on inferred union payload objects use the `synthetic` disposition. They
have no annotation surface and are reachable only after their parent resolver
field has passed its own required authorization disposition.

Required authorization coverage is a structural guarantee that checks are declared and generated. It does not prove policy correctness, authentication setup, or collection filtering. Authorization on a list gates access to the list field but does not scope its items, lengths, ordering, cursors, or existence. Filter collections in the data/domain layer, and keep authorization caches request-scoped unless they include every security-relevant identity, tenant, action, argument, and invalidation dimension.
