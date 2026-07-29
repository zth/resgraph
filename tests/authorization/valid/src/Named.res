@gql.authorize(Security.canReadNamed) @gql.interface
type named = {
  @gql.field
  name: string,
}
