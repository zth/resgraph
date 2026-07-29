@gql.type
type mutation

@gql.field
let outcomeOnly = (_: mutation): ResGraph.Authorization.outcome<
  string,
  string,
> => ResGraph.Authorization.Allowed("too late")
