@gql.interface
type publicNamed = {
  @gql.public({reason: "Names are public across this interface"}) @gql.field
  label: string,
}
