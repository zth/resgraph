@gql.interface
type entity = {
  @gql.field
  id: string,
}

@gql.type @gql.implements("Entity")
type sharedItem = {
  @gql.field
  id: string,
  @gql.field
  label: string,
}
