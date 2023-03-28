/** A user in the system. */
@gql.type
type user = {
  /** The users name.*/ name: string,
  @gql.field /** The age of the user. */
  age: int,
  @gql.field @deprecated("Use 'age' instead.") /** The last age of the user. */
  lastAge: option<int>,
}

/** A group in the system. */
@gql.type
type group = {
  /** The group name.*/
  @gql.field
  name: string,
  memberIds: array<string>,
}

/** A user or a group. */
@gql.union
type userOrGroup = | /** This is a user.*/ User(user) | /** And this is a group. */ Group(group)

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
  let id = (user: user) => {
    ("User:" ++ user.name)->ResGraph.id
  }

  @gql.field
  let name = (user: user, ~includeFullName) => {
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

  @gql.field
  let allNames = (user: user) => {
    [user.name]
  }
}

@gql.type
type query = {}

@gql.inputObject /** Configuration for searching for a user.*/
type userConfig = {
  /** The ID of a user to search for. */ id: string,
  /** The name of the user to search for. */
  @deprecated("This is going away")
  name?: string,
}

module QueryFields = {
  @gql.field
  let me = (_: query): option<user> => {
    Some({name: "Hello", age: 35, lastAge: None})
  }

  @gql.field
  let entity = (_: query, ~id: ResGraph.id, ~ctx: ResGraphContext.context) => {
    ignore(id)
    ignore(ctx)
    User({name: "Hello", age: 35, lastAge: None})
  }

  @gql.field
  let searchForUser = (_: query, ~input: userConfig): option<user> => {
    Some({name: input.name->Belt.Option.getWithDefault("Hello"), age: 35, lastAge: None})
  }

  @gql.field
  let allowExplicitNull = (_: query, ~someNullable) => {
    let wasNull = someNullable == Js.Nullable.null
    let wasUndefined = someNullable == Js.Nullable.undefined

    if wasNull {
      "Was null"
    } else if wasUndefined {
      "Was undefined"
    } else {
      someNullable->Js.Nullable.toOption->Belt.Option.getExn
    }
  }

  @gql.field
  let listAsArgs = (
    _: query,
    ~regularList,
    ~optionalList=?,
    ~nullableList,
    ~nullableInnerList,
    ~list1: option<array<option<string>>>,
    ~list2: option<array<option<array<option<string>>>>>,
    ~list3: option<array<option<array<array<string>>>>>,
  ) => {
    ignore(list1)
    ignore(list2)
    ignore(list3)
    let regularList = regularList->Belt.Array.keepMap(v => v)
    let optionalList = optionalList->Belt.Option.getWithDefault([])
    let nullableList =
      nullableList->Js.Nullable.toOption->Belt.Option.getWithDefault([])->Belt.Array.keepMap(v => v)
    let nullableInnerList =
      nullableInnerList
      ->Js.Nullable.toOption
      ->Belt.Option.getWithDefault([])
      ->Belt.Array.keepMap(Js.Nullable.toOption)

    let arr =
      Belt.Array.concatMany([
        regularList,
        optionalList,
        nullableList,
        nullableInnerList,
      ])->Js.Array2.map(str => "v " ++ str)

    arr
  }
}

// ^gen
