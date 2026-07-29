@gql.interface
type publicNamed = {
  @gql.authorizationUnchecked({reason: "Legacy interface field pending migration"}) @gql.field
  label: string,
}
