@@warning("-27")

open ResGraph__GraphQLJs

let enum_UserStatus = GraphQLEnumType.make({
  name: "UserStatus",
  description: "Indicates what status a user currently has.",
  values: {
    "Online": {
      GraphQLEnumType.value: "Online",
      description: "User is online.",
      deprecationReason: ?None,
    },
    "Offline": {
      GraphQLEnumType.value: "Offline",
      description: "User is offline.",
      deprecationReason: ?None,
    },
    "Idle": {
      GraphQLEnumType.value: "Idle",
      description: "User is idle.",
      deprecationReason: "Use 'Offline' instead.",
    },
  }->makeEnumValues,
})
let enum_userStatus = GraphQLEnumType.make({
  name: "userStatus",
  description: "Indicates what status a user currently has.",
  values: {
    "Online": {
      GraphQLEnumType.value: "Online",
      description: "User is online.",
      deprecationReason: ?None,
    },
    "Offline": {
      GraphQLEnumType.value: "Offline",
      description: "User is offline.",
      deprecationReason: ?None,
    },
    "Idle": {
      GraphQLEnumType.value: "Idle",
      description: "User is idle.",
      deprecationReason: "Use 'Offline' instead.",
    },
  }->makeEnumValues,
})
let t_User: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_User = () => t_User.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Query = () => t_Query.contents

t_User.contents = GraphQLObjectType.make({
  name: "User",
  description: "A user in the system.",
  fields: () =>
    {
      "currentStatus": {
        typ: enum_UserStatus->GraphQLEnumType.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx) => {Schema.UserFields.currentStatus(src)}),
      },
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"includeFullName": {typ: Scalars.boolean->Scalars.toGraphQLType}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          Schema.UserFields.name(
            src,
            ~includeFullName=args["includeFullName"]->Js.Nullable.toOption,
          )
        }),
      },
      "id": {
        typ: Scalars.id->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx) => {Schema.UserFields.id(src)}),
      },
      "age": {
        typ: Scalars.int->Scalars.toGraphQLType->nonNull,
        description: "The age of the user.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx) => src["age"]),
      },
      "lastAge": {
        typ: Scalars.int->Scalars.toGraphQLType,
        description: "The last age of the user.",
        deprecationReason: "Use 'age' instead.",
        resolve: makeResolveFn((src, _args, _ctx) => src["lastAge"]),
      },
    }->makeFields,
})
t_Query.contents = GraphQLObjectType.make({
  name: "Query",
  description: ?None,
  fields: () =>
    {
      "me": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx) => {Schema.QueryFields.me(src)}),
      },
    }->makeFields,
})

let schema = GraphQLSchemaType.make({"query": get_Query()})
