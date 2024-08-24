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
  // Single line comment in here...
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
type userOrGroup = | /** This is a user.*/ Usr(user) | /** And this is a group. */ Group(group)
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
    let includeFullName = includeFullName->Option.getOr(false)

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
  let me = async (_: query, ~ctx: ResGraphContext.context, ~info: ResGraph.resolveInfo) => {
    Console.log(info)
    switch ctx.currentUserId {
    | None => None
    | Some(userId) =>
      switch await ctx.dataLoaders.user.byId->DataLoader.load(userId) {
      | Ok(user) => Some(user)
      | Error(_error) => None
      }
    }
  }

  @gql.field
  let entity = (_: query, ~id: ResGraph.id, ~ctx: ResGraphContext.context) => {
    ignore(id)
    ignore(ctx)
    Usr({id: "234", name: "Hello", age: 35, lastAge: None})
  }

  @gql.field
  let searchForUser = (_: query, ~input: userConfig): option<user> => {
    Some({
      id: "123",
      name: input.name->Option.getOr("Hello"),
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
    let optionalList = optionalList->Option.getOr([])
    let nullableList = nullableList->Nullable.toOption->Option.getOr([])->Array.keepSome
    let nullableInnerList = nullableInnerList->Nullable.toOption->Option.getOr([])->Array.keepSome

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
    Some({id: "123", User.name, age: 35, lastAge: None})
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

@gql.type
type userEdge = {
  ...ResGraph.Connections.edge<user>,
}

type connectionExtra = {totalCountFromServer: int}

@gql.type
type userConnection = {
  ...ResGraph.Connections.connection<userEdge>,
  extra: connectionExtra,
}

/** The total count of edges available in the connection. */
@gql.field
let totalCount = (connection: userConnection) => {
  Some(connection.extra.totalCountFromServer)
}

@gql.field
let allUsers = (_: query, ~first, ~last, ~before, ~after): option<userConnection> => {
  let _users: array<user> = [{age: 12, id: "123", name: "Hello", lastAge: None}]
  let _: ResGraph.Connections.connectionArgs = {first, last, before, after}

  None
}

// ^gen

type g = group
//        ^hov  TODO: Dig

@gql.union
type inlineUnion =
  | Ok({message: string})
  | NotOk({
      /** Stuff */
      reason: string,
      /** Whether this is liked or not. */
      @deprecated("Use something else.")
      liked?: bool,
    })
  | User(user)

@gql.field
let inlineUnion = (_: query) => NotOk({reason: "No way Jose"})->Some

@gql.type
type someType = {@gql.field msg: string}

@gql.inputUnion
type paginationArgs =
  Forward({first?: int, after?: string}) | Backwards({last?: int, before?: string})

@gql.inputObject
type coordinates = {
  lat: float,
  lon: float,
}

@gql.inputObject
type address = {
  streetAdddress: string,
  postalCode: string,
  city: string,
}

@gql.inputUnion
type location =
  | ByCoordinates(coordinates)
  | ByAddress(address)
  | ByMagicString({text: string})
  | ById(ResGraph.id)

@gql.field
let findThing = (_: query, ~location: location) => {
  switch location {
  | ByCoordinates({lat}) => Some("coordinates! " ++ lat->Float.toString)
  | ByAddress({city}) => Some("address! " ++ city)
  | ByMagicString({text}) => Some("Magic string! " ++ text)
  | ById(id) => Some(id->ResGraph.idToString)
  }
}

@gql.field
let inferredEnum = (_: query, ~rawStatus) => {
  switch rawStatus {
  | "ONLINE" => #Online
  | "OFFLINE" => #Offline
  | _ => #Other
  }
}

@gql.field
let inferredEnumAsArg = (_: query, ~status) => {
  switch status {
  | #Online => Some("Online")
  | #Offline => Some("Offline")
  | _ => None
  }
}

@gql.type
type someOtherType = {@gql.field message: string}

@gql.field
let inferredUnion = (_: query, ~rawStatus) => {
  switch rawStatus {
  | "ONLINE" => #SomeType({msg: "Online"})
  | _ => #SomeOtherType({message: "Offline"})
  }
}

@gql.field
let inferredUnionWithInferredConstructor = (_: query, ~rawStatus) => {
  switch rawStatus {
  | "ONLINE" => #SomeType({msg: "Online"})->Some
  | _ =>
    #SomeInferredType({
      "message": "Offline",
      "someTypeStuff": Some({msg: "Online"}),
    })->Some
  }
}
