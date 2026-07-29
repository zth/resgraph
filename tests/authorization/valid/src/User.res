@gql.authorize(Security.canReadUser) @gql.type
type user = {
  @gql.field
  id: string,
  @gql.field
  secret: string,
}
