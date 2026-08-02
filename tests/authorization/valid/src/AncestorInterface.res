@gql.authorize.byAncestor({
  reason: "Interface fields rely on a protected concrete entry path",
})
@gql.interface
type ancestorNamed = {
  @gql.field
  name: string,
  @gql.authorize.byAncestor({
    reason: "Interface field relies on a protected concrete entry path",
  })
  @gql.field
  detail: string,
  @gql.field
  publicOverride: string,
}

@gql.implements("AncestorNamed") @gql.type
type protectedAncestorNamed = {
  @gql.field
  name: string,
  @gql.field
  detail: string,
  @gql.public({reason: "Concrete fields may override an inherited ancestor assertion"}) @gql.field
  publicOverride: string,
}

@gql.authorize(Security.first) @gql.field
let protectedAncestorNamed = (_: Query.query): protectedAncestorNamed => {
  name: "protected",
  detail: "detail",
  publicOverride: "public",
}

@gql.implements("AncestorNamed") @gql.type
type publicAncestorNamed = {
  @gql.public({reason: "This concrete implementation exposes public names"}) @gql.field
  name: string,
  @gql.public({reason: "This concrete implementation exposes public details"}) @gql.field
  detail: string,
  @gql.public({reason: "This concrete implementation exposes public overrides"}) @gql.field
  publicOverride: string,
}

@gql.public({reason: "This concrete implementation is intentionally public"}) @gql.field
let publicAncestorNamed = (_: Query.query): publicAncestorNamed => {
  name: "public",
  detail: "public",
  publicOverride: "public",
}

@gql.authorize.byAncestor({
  reason: "Parent interface fields propagate through child interfaces",
})
@gql.interface
type ancestorParent = {
  @gql.field
  inherited: string,
}

@gql.implements("AncestorParent") @gql.interface
type ancestorChild = {
  @gql.field
  inherited: string,
}

@gql.implements("AncestorChild") @gql.type
type nestedAncestorConcrete = {
  @gql.field
  inherited: string,
}

@gql.authorize(Security.first) @gql.field
let nestedAncestor = (_: Query.query): nestedAncestorConcrete => {inherited: "protected"}
