/** A user in the system. */
@gql.type
type user = {
  ...NodeInterface.node,
  ...HasNameInterface.hasName,
  /** The age of the user. */
  @gql.field
  age: int,
  /** The last age of the user. */
  @gql.field @deprecated("Use 'age' instead.")
  lastAge: option<int>,
}

let fromDbUser = (user: Db.userFromDb) => {
  id: user.id,
  name: user.name,
  age: user.age,
  lastAge: None,
}
