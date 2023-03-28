@@warning("-27")

open ResGraph__GraphQLJs

let typeUnwrapper: 'src => 'return = %raw(`function typeUnwrapper(src) { if (src == null) return null; if (typeof src === 'object' && src.hasOwnProperty('_0')) return src['_0']; return src;}`)
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
let t_User: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_User = () => t_User.contents
let t_Group: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Group = () => t_Group.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Query = () => t_Query.contents
let input_UserConfig: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_UserConfig = () => input_UserConfig.contents
let union_UserOrGroup: ref<GraphQLUnionType.t> = Obj.magic({"contents": Js.null})
let get_UserOrGroup = () => union_UserOrGroup.contents

let union_UserOrGroup_resolveType = (v: Schema.userOrGroup) =>
  switch v {
  | User(_) => get_User()
  | Group(_) => get_Group()
  }

t_User.contents = GraphQLObjectType.make({
  name: "User",
  description: "A user in the system.",
  fields: () =>
    {
      "allNames": {
        typ: GraphQLListType.make(
          Scalars.string->Scalars.toGraphQLType,
        )->GraphQLListType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.UserFields.allNames(src)
        }),
      },
      "currentStatus": {
        typ: enum_UserStatus->GraphQLEnumType.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.UserFields.currentStatus(src)
        }),
      },
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"includeFullName": {typ: Scalars.boolean->Scalars.toGraphQLType}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
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
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.UserFields.id(src)
        }),
      },
      "age": {
        typ: Scalars.int->Scalars.toGraphQLType->nonNull,
        description: "The age of the user.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx) => {
          let src = typeUnwrapper(src)
          src["age"]
        }),
      },
      "lastAge": {
        typ: Scalars.int->Scalars.toGraphQLType,
        description: "The last age of the user.",
        deprecationReason: "Use 'age' instead.",
        resolve: makeResolveFn((src, _args, _ctx) => {
          let src = typeUnwrapper(src)
          src["lastAge"]
        }),
      },
    }->makeFields,
})
t_Group.contents = GraphQLObjectType.make({
  name: "Group",
  description: "A group in the system.",
  fields: () =>
    {
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: "The group name.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx) => {
          let src = typeUnwrapper(src)
          src["name"]
        }),
      },
    }->makeFields,
})
t_Query.contents = GraphQLObjectType.make({
  name: "Query",
  description: ?None,
  fields: () =>
    {
      "searchForUser": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "input": {typ: get_UserConfig()->GraphQLInputObjectType.toGraphQLType->nonNull},
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.searchForUser(src, ~input=args["input"])
        }),
      },
      "entity": {
        typ: get_UserOrGroup()->GraphQLUnionType.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"id": {typ: Scalars.id->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.entity(src, ~id=args["id"], ~ctx)
        }),
      },
      "me": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.me(src)
        }),
      },
    }->makeFields,
})
input_UserConfig.contents = GraphQLInputObjectType.make({
  name: "UserConfig",
  description: "Configuration for searching for a user.",
  fields: () =>
    {
      "id": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: "The ID of a user to search for.",
        deprecationReason: ?None,
      },
      "name": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
        description: "The name of the user to search for.",
        deprecationReason: "This is going away",
      },
    }->makeFields,
})
union_UserOrGroup.contents = GraphQLUnionType.make({
  name: "UserOrGroup",
  description: "A user or a group.",
  types: () => [get_User(), get_Group()],
  resolveType: GraphQLUnionType.makeResolveUnionTypeFn(union_UserOrGroup_resolveType),
})

let schema = GraphQLSchemaType.make({"query": get_Query()})
