@@warning("-32")

@gql.interface
type named = {
  @gql.field name: string,
}

@gql.interface
type ranked = {
  @gql.field rank: int,
}

@gql.interface
type nullableNamed = {
  @gql.field nullableName: option<string>,
}

@gql.implements("Named")
@gql.interface
type namedEntity = {
  @gql.field name: string,
  @gql.field entityKind: string,
}

@gql.interface
type companyHolder = {
  @gql.field company: namedEntity,
}

@gql.interface
type searchable = {
  @gql.field id: string,
}

@gql.field
let label = (_: searchable, ~prefix: option<string>=?): string =>
  switch prefix {
  | Some(prefix) => prefix ++ ":searchable"
  | None => "searchable"
  }

@gql.interface
type contextual = {
  @gql.field id: string,
}

@gql.field
let contextLabel = (_: contextual, ~ctx: ResGraphContext.context): string =>
  switch ctx.currentUserId {
  | Some(id) => "ctx:" ++ id
  | None => "ctx:none"
  }

@gql.interface
type contextOverride = {
  @gql.field id: string,
}

@gql.field
let contextOverrideLabel = (_: contextOverride): string => "base"

@gql.implements("NamedEntity")
@gql.implements("NullableNamed")
@gql.implements("Ranked")
@gql.type
type explicitCompany = {
  @gql.field name: string,
  @gql.field entityKind: string,
  @gql.field nullableName: string,
  @gql.field rank: int,
  @gql.field headquarters: option<string>,
}

@gql.implements("CompanyHolder")
@gql.type
type explicitCompanyHolder = {
  @gql.field company: explicitCompany,
}

@gql.implements("Searchable")
@gql.type
type explicitSearchResult = {
  @gql.field id: string,
}

@gql.implements("Contextual")
@gql.type
type explicitContextResult = {
  @gql.field id: string,
}

@gql.field
let contextLabel = (_: explicitContextResult): string => "override-no-ctx"

@gql.implements("ContextOverride")
@gql.type
type explicitContextOverrideResult = {
  @gql.field id: string,
}

@gql.field
let contextOverrideLabel = (
  _: explicitContextOverrideResult,
  ~ctx: ResGraphContext.context,
): string =>
  switch ctx.currentUserId {
  | Some(id) => "override:" ++ id
  | None => "override:none"
  }

let makeExplicitCompany = (): explicitCompany => {
  name: "Informind",
  entityKind: "Company",
  nullableName: "Informind",
  rank: 1,
  headquarters: Some("Stockholm"),
}

@gql.field
let explicitCompany = (_: Query.query): explicitCompany => makeExplicitCompany()

@gql.field
let explicitCompanyHolder = (_: Query.query): explicitCompanyHolder => {
  company: makeExplicitCompany(),
}

@gql.field
let explicitSearchResult = (_: Query.query): explicitSearchResult => {id: "search-result"}

@gql.field
let explicitContextResult = (_: Query.query): explicitContextResult => {id: "ctx-result"}

@gql.field
let explicitContextOverrideResult = (_: Query.query): explicitContextOverrideResult => {
  id: "ctx-override-result",
}
