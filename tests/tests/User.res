/** A user in the system. */
@gql.type({interfaces: [HasNameInterface.hasName]})
type user = {
  /** The users name.*/ name: string,
  @gql.field /** The age of the user. */
  age: int,
  @gql.field @deprecated("Use 'age' instead.") /** The last age of the user. */
  lastAge: option<int>,
}
