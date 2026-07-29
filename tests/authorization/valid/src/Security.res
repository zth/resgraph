type reason = Denied
type findArgs = {"id": string}
type nothing = unit
let canReadUser = (_: User.user, ~args) => ResGraph.Authorization.Allowed()

let canFindUser = (
  _: Query.query,
  ~args: findArgs,
  ~ctx: ResGraphContext.context,
  ~info: ResGraph.resolveInfo,
): AuthTypes.authResult<nothing, reason> => {
  let _ = (args["id"], ctx, info)
  ResGraph.Authorization.Allowed()
}

let canLoadAsync = async (_: Query.query, ~args) => ResGraph.Authorization.Allowed()

let canMutate = (_: Mutation.mutation, ~args) => {
  let _ = args["value"]
  ResGraph.Authorization.Allowed()
}

let onForbidden = (_reason, ~ctx, ~info) => {
  let _ = (ctx, info)
  ResGraph.Authorization.makeError(~message="Not authorized", ~code="FORBIDDEN")
}

let canReadNamed = (_: Named.named, ~args) => ResGraph.Authorization.Allowed()

let canLoadDevice = (_: Query.query, ~args) => ResGraph.Authorization.Allowed()

let first = (_: Query.query, ~args) => ResGraph.Authorization.Allowed()

module Nested = {
  let second = (_: Query.query, ~args) => ResGraph.Authorization.Allowed()
}

let canLoadPublicDevice = (_: Query.query, ~args) => ResGraph.Authorization.Allowed()

let canLoadOutcomeDevice = (_: Query.query, ~args) => ResGraph.Authorization.Allowed()
