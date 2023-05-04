type user = User.user

/** A timestamp. "Testing quotes here". */
@gql.scalar
type timestamp = float
//    ^hov

@gql.scalar
type timestampList = array<float>

@gql.interface
type interfaceNobodyImplements = {@gql.field weirdField: string}
//                ^hov

/** A group in the system. */
@gql.type
type group = {
  ...NodeInterface.node,
  ...HasNameInterface.hasName,
  /** The group name.*/
  memberIds: array<string>,
  /** The timestamp when this group was created.*/
  @gql.field
  createdAt: option<timestamp>,
  /** When this group was last modified. */
  @gql.field
  modifiedAt: option<CustomScalars.Inner.TimestampHidden.t>,
}

@gql.type
type pet = {
  ...HasNameInterface.hasName,
  @gql.field
  age: int,
}

/** A user or a group. */
@gql.union
type userOrGroup = | /** This is a user.*/ User(user) | /** And this is a group. */ Group(group)
//           ^hov

/** Indicates what status a user currently has. */
@gql.enum
type userStatus =
  //        ^hov
  | /** User is online. */
  @as("ONLINE")
  Online
  | /** User is offline. */ Offline
  | /** User is idle. */
  @deprecated(`Use 'Offline' instead. This should be "escaped".`)
  Idle

module UserFields = {
  @gql.field
  let name = (user: user, ~includeFullName) => {
    let includeFullName = includeFullName->Option.getWithDefault(false)

    if includeFullName {
      user.name
    } else {
      user.name->String.slice(~start=0, ~end=3)
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

type query = Query.query

@gql.inputObject /** Additional for searching for a user.*/
type userConfigContext = {
  //  ^hov
  groupId: Js.Nullable.t<string>,
  name?: string,
}

@gql.inputObject /** Configuration for searching for a user.*/
type userConfig = {
  /** The ID of a user to search for. */ id: string,
  /** The name of the user to search for. */
  @deprecated("This is going away")
  name?: string,
  context?: userConfigContext,
}

module QueryFields = {
  @gql.field
  let me = async (_: query, ~ctx: ResGraphContext.context) => {
    let user = await ctx.loadCurrentUser()
    user
  }

  @gql.field
  let entity = (_: query, ~id: ResGraph.id, ~ctx: ResGraphContext.context) => {
    ignore(id)
    ignore(ctx)
    User({id: "234"->ResGraph.id, name: "Hello", age: 35, lastAge: None})
  }

  @gql.field
  let searchForUser = (_: query, ~input: userConfig): option<user> => {
    Some({
      id: "123"->ResGraph.id,
      name: input.name->Option.getWithDefault("Hello"),
      age: 35,
      lastAge: None,
    })
  }

  @gql.field
  let allowExplicitNull = (_: query, ~someNullable) => {
    let wasNull = someNullable == null
    let wasUndefined = someNullable == undefined

    if wasNull {
      "Was null"
    } else if wasUndefined {
      "Was undefined"
    } else {
      someNullable->Nullable.toOption->Option.getExn
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
    let regularList = regularList->Array.keepSome
    let optionalList = optionalList->Option.getWithDefault([])
    let nullableList = nullableList->Nullable.toOption->Option.getWithDefault([])->Array.keepSome
    let nullableInnerList =
      nullableInnerList->Nullable.toOption->Option.getWithDefault([])->Array.keepSome

    let arr =
      Array.flat([regularList, optionalList, nullableList, nullableInnerList])->Array.map(str =>
        "v " ++ str
      )

    arr
  }

  @gql.field
  let pet = (_: query) => {
    let pet: pet = {name: "Mr Pet", age: 12}
    Some(pet)
  }

  @gql.field
  let customScalar = (_: query): option<CustomScalars.TimestampZ.t> => {
    Some("123")
  }

  @gql.field
  let customScalarImplSerializable = (_: query): option<
    CustomScalars.TimestampHiddenSerializable.t,
  > => {
    "123"->CustomScalars.TimestampHiddenSerializable.make->Some
  }
}

@gql.type
type mutation = {}

module Mutations = {
  @gql.field
  let addUser = (_: mutation, ~name) => {
    // ^hov
    Some({id: "123"->ResGraph.id, User.name, age: 35, lastAge: None})
  }
}

@gql.field
let currentTime = (_: query) => {
  CustomScalars.Inner.TimestampHidden.parseValue(Number(Date.now()))
}

@gql.field
let currentTimeFlat = (_: query) => {
  let timestamp: timestamp = Date.now()
  timestamp
}

// ^gen

type g = group
//        ^hov  TODO: Dig
