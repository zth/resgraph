@gql.authorize(Security.canReadUser) @gql.type
type user = {
  @gql.field
  id: string,
  @gql.authorize(Security.canReadUser) @gql.authorize(Security.canReadUser) @gql.field
  secret: string,
}
