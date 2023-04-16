/** A user in the system. */
@gql.type
type user = {
  ...NodeInterface.node,
  ...HasNameInterface.hasName,
  @gql.field /** The age of the user. */
  age: int,
  @gql.field @deprecated("Use 'age' instead.") /** The last age of the user. */
  lastAge: option<int>,
}
