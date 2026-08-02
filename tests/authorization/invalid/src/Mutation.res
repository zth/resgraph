@gql.type
type mutation

@gql.authorize.byAncestor({reason: "Mutation payloads require a pre-resolver policy"}) @gql.type
type outcomeOnlyPayload = {
  @gql.field
  value: string,
}

@gql.field
let outcomeOnly = (_: mutation): ResGraph.Authorization.outcome<
  outcomeOnlyPayload,
  string,
> => ResGraph.Authorization.Allowed({value: "too late"})
