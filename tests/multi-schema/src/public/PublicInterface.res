@gql.field
let entity = (_: Public.query): PublicSchema__Interface_entity.Resolver.t => SharedItem({
  id: "public-entity",
  label: "Public entity",
})
