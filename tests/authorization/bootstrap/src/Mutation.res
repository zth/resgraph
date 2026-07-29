@gql.type
type mutation

@gql.field
let legacyMutation = (_: mutation): ResGraph.Authorization.outcome<
  string,
  string,
> => ResGraph.Authorization.Allowed("updated")
