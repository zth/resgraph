@gql.field
let entity = (_: Admin.query): AdminSchema__Interface_entity.Resolver.t => SharedItem({
  id: "admin-entity",
  label: "Admin entity",
})
