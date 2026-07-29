type reason = Denied
exception ForbiddenError

let canReadUser = (_: User.user, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let canFindUser = (
  _: Query.query,
  ~args: {"id": string},
  ~ctx: ResGraphContext.context,
  ~info: ResGraph.resolveInfo,
): ResGraph.Authorization.outcome<unit, reason> => {
  let _ = (args["id"], ctx, info)
  ResGraph.Authorization.Allowed()
}

let canLoadAsync = async (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let canMutate = (_: Mutation.mutation, ~args: {"value": string}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => {
  let _ = args["value"]
  ResGraph.Authorization.Allowed()
}

let onForbidden = (_reason, ~ctx: ResGraphContext.context, ~info: ResGraph.resolveInfo): exn => {
  let _ = (ctx, info)
  ForbiddenError
}

let canReadNamed = (_: Named.named, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let canLoadDevice = (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let first = (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

module Nested = {
  let second = (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
    unit,
    reason,
  > => ResGraph.Authorization.Allowed()
}

let canLoadPublicDevice = (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let canLoadOutcomeDevice = (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()
