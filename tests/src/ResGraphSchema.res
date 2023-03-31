@@warning("-27")

open ResGraph__GraphQLJs

let typeUnwrapper: 'src => 'return = %raw(`function typeUnwrapper(src) { if (src == null) return null; if (typeof src === 'object' && src.hasOwnProperty('_0')) return src['_0']; return src;}`)
type inputObjectFieldConverterFn
external makeInputObjectFieldConverterFn: ('a => 'b) => inputObjectFieldConverterFn = "%identity"

let applyConversionToInputObject: (
  'a,
  array<(string, inputObjectFieldConverterFn)>,
) => 'a = %raw(`function applyConversionToInputObject(obj, instructions) {
      if (instructions.length === 0) return obj;
      let newObj = Object.assign({}, obj);
      instructions.forEach(instruction => {
        let value = newObj[instruction[0]];
         newObj[instruction[0]] = instruction[1](value);
      })
      return newObj;
    }`)

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
let i_HasName: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": Js.null})
let get_HasName = () => i_HasName.contents
let t_Mutation: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Mutation = () => t_Mutation.contents
let t_User: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_User = () => t_User.contents
let t_Group: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Group = () => t_Group.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Query = () => t_Query.contents
let input_UserConfig: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_UserConfig = () => input_UserConfig.contents
let input_UserConfig_conversionInstructions = []
let input_UserConfigContext: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_UserConfigContext = () => input_UserConfigContext.contents
let input_UserConfigContext_conversionInstructions = []
input_UserConfig_conversionInstructions->Array.pushMany([
  ("name", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  (
    "context",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) =>
        v->applyConversionToInputObject(input_UserConfigContext_conversionInstructions)->Some
      }
    ),
  ),
])
input_UserConfigContext_conversionInstructions->Array.pushMany([
  ("name", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
let union_UserOrGroup: ref<GraphQLUnionType.t> = Obj.magic({"contents": Js.null})
let get_UserOrGroup = () => union_UserOrGroup.contents

let union_UserOrGroup_resolveType = (v: Schema.userOrGroup) =>
  switch v {
  | User(_) => get_User()
  | Group(_) => get_Group()
  }

i_HasName.contents = GraphQLInterfaceType.make({
  name: "HasName",
  description: "An entity with a name.",
  interfaces: [],
  fields: () =>
    {
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx) => {
          let src = typeUnwrapper(src)
          src["name"]
        }),
      },
    }->makeFields,
})
t_Mutation.contents = GraphQLObjectType.make({
  name: "Mutation",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "addUser": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {"name": {typ: Scalars.string->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.Mutations.addUser(src, ~name=args["name"])
        }),
      },
    }->makeFields,
})
t_User.contents = GraphQLObjectType.make({
  name: "User",
  description: "A user in the system.",
  interfaces: [get_HasName()],
  fields: () =>
    {
      "allNames": {
        typ: GraphQLListType.make(Scalars.string->Scalars.toGraphQLType->nonNull)
        ->GraphQLListType.toGraphQLType
        ->nonNull,
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
          Schema.UserFields.name(src, ~includeFullName=args["includeFullName"]->Nullable.toOption)
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
  interfaces: [get_HasName()],
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
  interfaces: [],
  fields: () =>
    {
      "listAsArgs": {
        typ: GraphQLListType.make(Scalars.string->Scalars.toGraphQLType->nonNull)
        ->GraphQLListType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "regularList": {
            typ: GraphQLListType.make(Scalars.string->Scalars.toGraphQLType)
            ->GraphQLListType.toGraphQLType
            ->nonNull,
          },
          "optionalList": {
            typ: GraphQLListType.make(
              Scalars.string->Scalars.toGraphQLType->nonNull,
            )->GraphQLListType.toGraphQLType,
          },
          "nullableList": {
            typ: GraphQLListType.make(
              Scalars.string->Scalars.toGraphQLType,
            )->GraphQLListType.toGraphQLType,
          },
          "nullableInnerList": {
            typ: GraphQLListType.make(
              Scalars.string->Scalars.toGraphQLType,
            )->GraphQLListType.toGraphQLType,
          },
          "list1": {
            typ: GraphQLListType.make(
              Scalars.string->Scalars.toGraphQLType,
            )->GraphQLListType.toGraphQLType,
          },
          "list2": {
            typ: GraphQLListType.make(
              GraphQLListType.make(
                Scalars.string->Scalars.toGraphQLType,
              )->GraphQLListType.toGraphQLType,
            )->GraphQLListType.toGraphQLType,
          },
          "list3": {
            typ: GraphQLListType.make(
              GraphQLListType.make(
                GraphQLListType.make(Scalars.string->Scalars.toGraphQLType->nonNull)
                ->GraphQLListType.toGraphQLType
                ->nonNull,
              )->GraphQLListType.toGraphQLType,
            )->GraphQLListType.toGraphQLType,
          },
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.listAsArgs(
            src,
            ~regularList=args["regularList"]->Array.map(v => v->Nullable.toOption),
            ~optionalList=?args["optionalList"]->Nullable.toOption,
            ~nullableList=args["nullableList"]->Nullable.map(v =>
              v->Array.map(v => v->Nullable.toOption)
            ),
            ~nullableInnerList=args["nullableInnerList"]->Nullable.map(v =>
              v->Array.map(v => v->Nullable.toOption)
            ),
            ~list1=switch args["list1"]->Nullable.toOption {
            | None => None
            | Some(v) => v->Array.map(v => v->Nullable.toOption)->Some
            },
            ~list2=switch args["list2"]->Nullable.toOption {
            | None => None
            | Some(v) =>
              v
              ->Array.map(v =>
                switch v->Nullable.toOption {
                | None => None
                | Some(v) => v->Array.map(v => v->Nullable.toOption)->Some
                }
              )
              ->Some
            },
            ~list3=switch args["list3"]->Nullable.toOption {
            | None => None
            | Some(v) => v->Array.map(v => v->Nullable.toOption)->Some
            },
          )
        }),
      },
      "allowExplicitNull": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"someNullable": {typ: Scalars.string->Scalars.toGraphQLType}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.allowExplicitNull(src, ~someNullable=args["someNullable"])
        }),
      },
      "searchForUser": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "input": {typ: get_UserConfig()->GraphQLInputObjectType.toGraphQLType->nonNull},
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.searchForUser(
            src,
            ~input=args["input"]->applyConversionToInputObject(
              input_UserConfig_conversionInstructions,
            ),
          )
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
      "context": {
        GraphQLInputObjectType.typ: get_UserConfigContext()->GraphQLInputObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
input_UserConfigContext.contents = GraphQLInputObjectType.make({
  name: "UserConfigContext",
  description: "Additional for searching for a user.",
  fields: () =>
    {
      "groupId": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "name": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
union_UserOrGroup.contents = GraphQLUnionType.make({
  name: "UserOrGroup",
  description: "A user or a group.",
  types: () => [get_User(), get_Group()],
  resolveType: GraphQLUnionType.makeResolveUnionTypeFn(union_UserOrGroup_resolveType),
})

let schema = GraphQLSchemaType.make({"query": get_Query(), "mutation": get_Mutation()})
