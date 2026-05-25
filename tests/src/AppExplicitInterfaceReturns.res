@gql.field
let explicitNamed = (_: Query.query): Interface_named.Resolver.t =>
  ExplicitCompany(AppExplicitInterfaceImplements.makeExplicitCompany())

@gql.field
let explicitSearchable = (_: Query.query): Interface_searchable.Resolver.t =>
  ExplicitSearchResult({id: "search-result"})
