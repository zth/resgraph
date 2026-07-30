@gql.field
let aliasedOutcome = (_: Query.query): AuthTypes.authResult<
  string,
  string,
> => ResGraph.Authorization.Allowed("allowed alias")

@gql.field
let aliasedAsyncOutcome = (_: Query.query): AuthTypes.asyncAuthResult<string, string> =>
  Promise.resolve(ResGraph.Authorization.Allowed("allowed async alias"))
