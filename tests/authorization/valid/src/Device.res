@gql.implements("Named") @gql.type
type device = {
  @gql.field
  name: string,
  @gql.field
  serial: string,
}
