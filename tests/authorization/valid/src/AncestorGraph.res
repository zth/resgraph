@gql.authorize.byAncestor({reason: "Union member fields are governed by the query policy"})
@gql.type
type ancestorAlpha = {
  @gql.field
  value: string,
}

@gql.authorize.byAncestor({reason: "Union member fields are governed by the query policy"})
@gql.type
type ancestorBeta = {
  @gql.field
  value: string,
}

@gql.union
type ancestorResult = Alpha(ancestorAlpha) | Beta(ancestorBeta)

@gql.authorize(Security.first) @gql.field
let ancestorUnion = (_: Query.query): ancestorResult => Alpha({value: "alpha"})

@gql.authorize.byAncestor({reason: "Shared result fields are protected on every entry path"})
@gql.type
type sharedProtected = {
  @gql.field
  value: string,
}

@gql.authorize(Security.first) @gql.field
let firstShared = (_: Query.query): sharedProtected => {value: "first"}

@gql.authorize(Security.first) @gql.field
let secondShared = (_: Query.query): sharedProtected => {value: "second"}
