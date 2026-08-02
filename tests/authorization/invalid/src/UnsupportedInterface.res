@gql.authorize.byAncestor({reason: "Interface fields rely on an upstream policy"}) @gql.interface
type unsupportedAncestor = {
  @gql.authorize.byAncestor({reason: "Interface field relies on an upstream policy"}) @gql.field
  value: string,
}

@gql.implements("UnsupportedAncestor") @gql.type
type unsupportedConcrete = {
  @gql.field
  value: string,
}
