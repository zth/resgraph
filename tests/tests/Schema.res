/** A user in the system. */
@gql.type
type user = {
  /** The users name.*/ name: string,
  @gql.field /** The age of the user. */
  age: int,
  @gql.field @deprecated("Use 'age' instead.") /** The last age of the user. */
  lastAge: option<int>,
}

/** Indicates what status a user currently has. */
@gql.enum
type userStatus =
  | /** User is online. */ Online
  | /** User is offline. */ Offline
  | /** User is idle. */
  @deprecated("Use 'Offline' instead.")
  Idle

module UserFields = {
  @gql.field
  let id = user => {
    ("User:" ++ user.name)->ResGraph.id
  }

  @gql.field
  let name = (user, ~includeFullName) => {
    let includeFullName = includeFullName->Belt.Option.getWithDefault(false)

    if includeFullName {
      user.name
    } else {
      user.name->Js.String2.slice(~from=0, ~to_=3)
    }
  }

  @gql.field
  let currentStatus = (_user: user) => {
    Online
  }
}

@gql.type
type query = {}

module QueryFields = {
  @gql.field
  let me = (_: query): option<user> => {
    Some({name: "Hello", age: 35, lastAge: None})
  }
}

// ^gen
