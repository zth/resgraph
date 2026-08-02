@gql.authorize.byAncestor({
  reason: "Interface fields require a protected concrete entry path",
})
@gql.interface
type unsupportedAncestor = {
  @gql.field
  value: string,
}

@gql.implements("UnsupportedAncestor") @gql.type
type unsupportedConcrete = {
  @gql.field
  value: string,
}

@gql.public({reason: "This unprotected path must invalidate the interface assertion"}) @gql.field
let unsupportedInterfaceAncestor = (_: Query.query): unsupportedConcrete => {value: "unsafe"}
