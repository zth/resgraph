@@warning("-27-32")

open ResGraph__GraphQLJs

let typeUnwrapper: 'src => 'return = %raw(`function typeUnwrapper(src) { if (src == null) return null; if (typeof src === 'object' && src.hasOwnProperty('_0')) return src['_0']; if (typeof src === 'object' && src.hasOwnProperty('VAL')) return src['VAL']; return src;}`)
let inputUnionUnwrapper: (
  'src,
  array<string>,
) => 'return = %raw(`function inputUnionUnwrapper(src, inlineRecordTypenames) {
      if (src == null) return null;
    
      let targetKey = null;
      let targetValue = null;
    
      Object.entries(src).forEach(([key, value]) => {
        if (value != null) {
          targetKey = key;
          targetValue = value;
        }
      });
    
      if (targetKey != null && targetValue != null) {
        let tagName = targetKey.slice(0, 1).toUpperCase() + targetKey.slice(1);
    
        if (inlineRecordTypenames.includes(tagName)) {
          return Object.assign({ TAG: tagName }, targetValue);
        }
    
        return {
          TAG: tagName,
          _0: targetValue,
        };
      }
    
      return null;
    }
    `)
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

let scalar_TimestampHidden = GraphQLScalar.make({
  let config: GraphQLScalar.config<CustomScalars.Inner.TimestampHidden.t> = {
    name: "TimestampHidden",
    description: "A timestamp, but with the implementation hidden in the server.",
    parseValue: CustomScalars.Inner.TimestampHidden.parseValue,
    serialize: CustomScalars.Inner.TimestampHidden.serialize,
  }
  config
})
let scalar_TimestampHiddenSerializable = GraphQLScalar.make({
  name: "TimestampHiddenSerializable",
  description: "A timestamp, but with the implementation hidden in the server. Under the\n    hood, it's serializable.",
})
let scalar_TimestampZ = GraphQLScalar.make({
  let config: GraphQLScalar.config<CustomScalars.TimestampZ.t> = {
    name: "TimestampZ",
    description: ?None,
    parseValue: CustomScalars.TimestampZ.parseValue,
    serialize: CustomScalars.TimestampZ.serialize,
  }
  config
})
let scalar_Timestamp = GraphQLScalar.make({
  name: "Timestamp",
  description: "A timestamp. \"Testing quotes here\".",
})
let scalar_TimestampList = GraphQLScalar.make({name: "TimestampList", description: ?None})
let enum_InferredEnum = GraphQLEnumType.make({
  name: "InferredEnum",
  description: ?None,
  values: {
    "Offline": {GraphQLEnumType.value: "Offline", description: ?None, deprecationReason: ?None},
    "Online": {GraphQLEnumType.value: "Online", description: ?None, deprecationReason: ?None},
    "Other": {GraphQLEnumType.value: "Other", description: ?None, deprecationReason: ?None},
  }->makeEnumValues,
})
let enum_InferredEnumAsArgStatus = GraphQLEnumType.make({
  name: "InferredEnumAsArgStatus",
  description: ?None,
  values: {
    "Offline": {GraphQLEnumType.value: "Offline", description: ?None, deprecationReason: ?None},
    "Online": {GraphQLEnumType.value: "Online", description: ?None, deprecationReason: ?None},
  }->makeEnumValues,
})
let enum_UserStatus = GraphQLEnumType.make({
  name: "UserStatus",
  description: "Indicates what status a user currently has.",
  values: {
    "ONLINE": {
      GraphQLEnumType.value: "ONLINE",
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
      deprecationReason: "Use 'Offline' instead. This should be \"escaped\".",
    },
  }->makeEnumValues,
})
let i_HasName: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": Js.null})
let get_HasName = () => i_HasName.contents
let i_Node: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": Js.null})
let get_Node = () => i_Node.contents
let t_InferredUnionWithInferredConstructorSomeInferredType: ref<GraphQLObjectType.t> = Obj.magic({
  "contents": Js.null,
})
let get_InferredUnionWithInferredConstructorSomeInferredType = () =>
  t_InferredUnionWithInferredConstructorSomeInferredType.contents
let t_InlineUnionNotOk: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_InlineUnionNotOk = () => t_InlineUnionNotOk.contents
let t_InlineUnionOk: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_InlineUnionOk = () => t_InlineUnionOk.contents
let t_Group: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Group = () => t_Group.contents
let t_Mutation: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Mutation = () => t_Mutation.contents
let t_PageInfo: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_PageInfo = () => t_PageInfo.contents
let t_Pet: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Pet = () => t_Pet.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Query = () => t_Query.contents
let t_SomeOtherType: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_SomeOtherType = () => t_SomeOtherType.contents
let t_SomeType: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_SomeType = () => t_SomeType.contents
let t_User: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_User = () => t_User.contents
let t_UserConnection: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_UserConnection = () => t_UserConnection.contents
let t_UserEdge: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_UserEdge = () => t_UserEdge.contents
let input_LocationByMagicString: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_LocationByMagicString = () => input_LocationByMagicString.contents
let input_LocationByMagicString_conversionInstructions = []
let input_PaginationArgsBackwards: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_PaginationArgsBackwards = () => input_PaginationArgsBackwards.contents
let input_PaginationArgsBackwards_conversionInstructions = []
let input_PaginationArgsForward: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_PaginationArgsForward = () => input_PaginationArgsForward.contents
let input_PaginationArgsForward_conversionInstructions = []
let input_Address: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_Address = () => input_Address.contents
let input_Address_conversionInstructions = []
let input_Coordinates: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_Coordinates = () => input_Coordinates.contents
let input_Coordinates_conversionInstructions = []
let input_UserConfig: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_UserConfig = () => input_UserConfig.contents
let input_UserConfig_conversionInstructions = []
let input_UserConfigContext: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_UserConfigContext = () => input_UserConfigContext.contents
let input_UserConfigContext_conversionInstructions = []
input_LocationByMagicString_conversionInstructions->Array.pushMany([])
input_PaginationArgsBackwards_conversionInstructions->Array.pushMany([
  ("last", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("before", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
input_PaginationArgsForward_conversionInstructions->Array.pushMany([
  ("first", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("after", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
input_Address_conversionInstructions->Array.pushMany([])
input_Coordinates_conversionInstructions->Array.pushMany([])
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
let union_InferredUnion: ref<GraphQLUnionType.t> = Obj.magic({"contents": Js.null})
let get_InferredUnion = () => union_InferredUnion.contents
let union_InferredUnionWithInferredConstructor: ref<GraphQLUnionType.t> = Obj.magic({
  "contents": Js.null,
})
let get_InferredUnionWithInferredConstructor = () =>
  union_InferredUnionWithInferredConstructor.contents
let union_InlineUnion: ref<GraphQLUnionType.t> = Obj.magic({"contents": Js.null})
let get_InlineUnion = () => union_InlineUnion.contents
let union_UserOrGroup: ref<GraphQLUnionType.t> = Obj.magic({"contents": Js.null})
let get_UserOrGroup = () => union_UserOrGroup.contents
let inputUnion_Location: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_Location = () => inputUnion_Location.contents
let inputUnion_Location_conversionInstructions = []
let inputUnion_PaginationArgs: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_PaginationArgs = () => inputUnion_PaginationArgs.contents
let inputUnion_PaginationArgs_conversionInstructions = []
inputUnion_Location_conversionInstructions->Array.pushMany([
  (
    "byCoordinates",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) => v->applyConversionToInputObject(input_Coordinates_conversionInstructions)->Some
      }
    ),
  ),
  (
    "byAddress",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) => v->applyConversionToInputObject(input_Address_conversionInstructions)->Some
      }
    ),
  ),
  (
    "byMagicString",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) =>
        v->applyConversionToInputObject(input_LocationByMagicString_conversionInstructions)->Some
      }
    ),
  ),
  ("byId", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_PaginationArgs_conversionInstructions->Array.pushMany([
  (
    "forward",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) =>
        v->applyConversionToInputObject(input_PaginationArgsForward_conversionInstructions)->Some
      }
    ),
  ),
  (
    "backwards",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) =>
        v->applyConversionToInputObject(input_PaginationArgsBackwards_conversionInstructions)->Some
      }
    ),
  ),
])

let union_InferredUnion_resolveType = v =>
  switch v {
  | #SomeOtherType(_) => "SomeOtherType"
  | #SomeType(_) => "SomeType"
  }

let union_InferredUnionWithInferredConstructor_resolveType = v =>
  switch v {
  | #SomeInferredType(_) => "InferredUnionWithInferredConstructorSomeInferredType"
  | #SomeType(_) => "SomeType"
  }

let union_InlineUnion_resolveType = (v: Schema.inlineUnion) =>
  switch v {
  | Ok(_) => "InlineUnionOk"
  | NotOk(_) => "InlineUnionNotOk"
  | User(_) => "User"
  }

let union_UserOrGroup_resolveType = (v: Schema.userOrGroup) =>
  switch v {
  | Usr(_) => "User"
  | Group(_) => "Group"
  }

let interface_HasName_resolveType = (v: Interface_hasName.Resolver.t) =>
  switch v {
  | Pet(_) => "Pet"
  | Group(_) => "Group"
  | User(_) => "User"
  }

let interface_Node_resolveType = (v: Interface_node.Resolver.t) =>
  switch v {
  | Group(_) => "Group"
  | User(_) => "User"
  }

i_HasName.contents = GraphQLInterfaceType.make({
  name: "HasName",
  description: "An entity with a name.",
  interfaces: [],
  fields: () =>
    {
      "abbreviatedName": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_HasName_resolveType),
})
i_Node.contents = GraphQLInterfaceType.make({
  name: "Node",
  description: "An object with an ID",
  interfaces: [],
  fields: () =>
    {
      "id": {
        typ: Scalars.id->Scalars.toGraphQLType->nonNull,
        description: "The id of the object.",
        deprecationReason: ?None,
      },
    }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Node_resolveType),
})
t_InferredUnionWithInferredConstructorSomeInferredType.contents = GraphQLObjectType.make({
  name: "InferredUnionWithInferredConstructorSomeInferredType",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "message": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["message"]
        }),
      },
      "someTypeStuff": {
        typ: get_SomeType()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["someTypeStuff"]
        }),
      },
    }->makeFields,
})
t_InlineUnionNotOk.contents = GraphQLObjectType.make({
  name: "InlineUnionNotOk",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "liked": {
        typ: Scalars.boolean->Scalars.toGraphQLType,
        description: "Whether this is liked or not.",
        deprecationReason: "Use something else.",
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["liked"]
        }),
      },
      "reason": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: "Stuff",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["reason"]
        }),
      },
    }->makeFields,
})
t_InlineUnionOk.contents = GraphQLObjectType.make({
  name: "InlineUnionOk",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "message": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["message"]
        }),
      },
    }->makeFields,
})
t_Group.contents = GraphQLObjectType.make({
  name: "Group",
  description: "A group in the system.",
  interfaces: [get_HasName(), get_Node()],
  fields: () =>
    {
      "abbreviatedName": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          HasNameInterfaceResolvers.abbreviatedName(src, ~typeName=Group)
        }),
      },
      "createdAt": {
        typ: scalar_Timestamp->GraphQLScalar.toGraphQLType,
        description: "The timestamp when this group was created.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["createdAt"]
        }),
      },
      "id": {
        typ: Scalars.id->Scalars.toGraphQLType->nonNull,
        description: "The id of the object.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          NodeInterfaceResolver.id(src, ~typename=Group)
        }),
      },
      "modifiedAt": {
        typ: scalar_TimestampHidden->GraphQLScalar.toGraphQLType,
        description: "When this group was last modified.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["modifiedAt"]
        }),
      },
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
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
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.Mutations.addUser(src, ~name=args["name"])
        }),
      },
    }->makeFields,
})
t_PageInfo.contents = GraphQLObjectType.make({
  name: "PageInfo",
  description: "Information about pagination in a connection.",
  interfaces: [],
  fields: () =>
    {
      "endCursor": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: "When paginating forwards, the cursor to continue.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["endCursor"]
        }),
      },
      "hasNextPage": {
        typ: Scalars.boolean->Scalars.toGraphQLType->nonNull,
        description: "When paginating forwards, are there more items?",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["hasNextPage"]
        }),
      },
      "hasPreviousPage": {
        typ: Scalars.boolean->Scalars.toGraphQLType->nonNull,
        description: "When paginating backwards, are there more items?",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["hasPreviousPage"]
        }),
      },
      "startCursor": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: "When paginating backwards, the cursor to continue.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["startCursor"]
        }),
      },
    }->makeFields,
})
t_Pet.contents = GraphQLObjectType.make({
  name: "Pet",
  description: ?None,
  interfaces: [get_HasName()],
  fields: () =>
    {
      "abbreviatedName": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          HasNameInterfaceResolvers.abbreviatedName(src, ~typeName=Pet)
        }),
      },
      "age": {
        typ: Scalars.int->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["age"]
        }),
      },
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
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
      "allUsers": {
        typ: get_UserConnection()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "after": {typ: Scalars.string->Scalars.toGraphQLType},
          "before": {typ: Scalars.string->Scalars.toGraphQLType},
          "first": {typ: Scalars.int->Scalars.toGraphQLType},
          "last": {typ: Scalars.int->Scalars.toGraphQLType},
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.allUsers(
            src,
            ~after=args["after"]->Nullable.toOption,
            ~before=args["before"]->Nullable.toOption,
            ~first=args["first"]->Nullable.toOption,
            ~last=args["last"]->Nullable.toOption,
          )
        }),
      },
      "allowExplicitNull": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"someNullable": {typ: Scalars.string->Scalars.toGraphQLType}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.allowExplicitNull(src, ~someNullable=args["someNullable"])
        }),
      },
      "currentTime": {
        typ: scalar_TimestampHidden->GraphQLScalar.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.currentTime(src)
        }),
      },
      "currentTimeFlat": {
        typ: scalar_Timestamp->GraphQLScalar.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.currentTimeFlat(src)
        }),
      },
      "customScalar": {
        typ: scalar_TimestampZ->GraphQLScalar.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.customScalar(src)
        }),
      },
      "customScalarImplSerializable": {
        typ: scalar_TimestampHiddenSerializable->GraphQLScalar.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.customScalarImplSerializable(src)
        }),
      },
      "entity": {
        typ: get_UserOrGroup()->GraphQLUnionType.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"id": {typ: Scalars.id->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.entity(src, ~ctx, ~id=args["id"])
        }),
      },
      "findThing": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "location": {typ: get_Location()->GraphQLInputObjectType.toGraphQLType->nonNull},
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.findThing(
            src,
            ~location=args["location"]
            ->applyConversionToInputObject(inputUnion_Location_conversionInstructions)
            ->inputUnionUnwrapper(["ByMagicString"]),
          )
        }),
      },
      "hasName": {
        typ: get_HasName()->GraphQLInterfaceType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {"id": {typ: Scalars.id->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          HasNameInterfaceResolvers.hasName(src, ~id=args["id"])
        }),
      },
      "inferredEnum": {
        typ: enum_InferredEnum->GraphQLEnumType.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"rawStatus": {typ: Scalars.string->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.inferredEnum(src, ~rawStatus=args["rawStatus"])
        }),
      },
      "inferredEnumAsArg": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "status": {typ: enum_InferredEnumAsArgStatus->GraphQLEnumType.toGraphQLType->nonNull},
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.inferredEnumAsArg(src, ~status=args["status"])
        }),
      },
      "inferredUnion": {
        typ: get_InferredUnion()->GraphQLUnionType.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"rawStatus": {typ: Scalars.string->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.inferredUnion(src, ~rawStatus=args["rawStatus"])
        }),
      },
      "inferredUnionWithInferredConstructor": {
        typ: get_InferredUnionWithInferredConstructor()->GraphQLUnionType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {"rawStatus": {typ: Scalars.string->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.inferredUnionWithInferredConstructor(src, ~rawStatus=args["rawStatus"])
        }),
      },
      "inlineUnion": {
        typ: get_InlineUnion()->GraphQLUnionType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.inlineUnion(src)
        }),
      },
      "listAsArgs": {
        typ: GraphQLListType.make(Scalars.string->Scalars.toGraphQLType->nonNull)
        ->GraphQLListType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {
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
          "nullableInnerList": {
            typ: GraphQLListType.make(
              Scalars.string->Scalars.toGraphQLType,
            )->GraphQLListType.toGraphQLType,
          },
          "nullableList": {
            typ: GraphQLListType.make(
              Scalars.string->Scalars.toGraphQLType,
            )->GraphQLListType.toGraphQLType,
          },
          "optionalList": {
            typ: GraphQLListType.make(
              Scalars.string->Scalars.toGraphQLType->nonNull,
            )->GraphQLListType.toGraphQLType,
          },
          "regularList": {
            typ: GraphQLListType.make(Scalars.string->Scalars.toGraphQLType)
            ->GraphQLListType.toGraphQLType
            ->nonNull,
          },
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.listAsArgs(
            src,
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
            ~nullableInnerList=args["nullableInnerList"]->Nullable.map(v =>
              v->Array.map(v => v->Nullable.toOption)
            ),
            ~nullableList=args["nullableList"]->Nullable.map(v =>
              v->Array.map(v => v->Nullable.toOption)
            ),
            ~optionalList=?args["optionalList"]->Nullable.toOption,
            ~regularList=args["regularList"]->Array.map(v => v->Nullable.toOption),
          )
        }),
      },
      "me": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.me(src, ~ctx, ~info)
        }),
      },
      "node": {
        typ: get_Node()->GraphQLInterfaceType.toGraphQLType,
        description: "Fetches an object given its ID.",
        deprecationReason: ?None,
        args: {"id": {typ: Scalars.id->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          NodeInterfaceResolver.node(src, ~ctx, ~id=args["id"])
        }),
      },
      "nodes": {
        typ: GraphQLListType.make(get_Node()->GraphQLInterfaceType.toGraphQLType)
        ->GraphQLListType.toGraphQLType
        ->nonNull,
        description: "Fetches objects given their IDs.",
        deprecationReason: ?None,
        args: {
          "ids": {
            typ: GraphQLListType.make(Scalars.id->Scalars.toGraphQLType->nonNull)
            ->GraphQLListType.toGraphQLType
            ->nonNull,
          },
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          NodeInterfaceResolver.nodes(src, ~ctx, ~ids=args["ids"])
        }),
      },
      "pet": {
        typ: get_Pet()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.pet(src)
        }),
      },
      "searchForUser": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "input": {typ: get_UserConfig()->GraphQLInputObjectType.toGraphQLType->nonNull},
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.QueryFields.searchForUser(
            src,
            ~input=args["input"]->applyConversionToInputObject(
              input_UserConfig_conversionInstructions,
            ),
          )
        }),
      },
    }->makeFields,
})
t_SomeOtherType.contents = GraphQLObjectType.make({
  name: "SomeOtherType",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "message": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["message"]
        }),
      },
    }->makeFields,
})
t_SomeType.contents = GraphQLObjectType.make({
  name: "SomeType",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "msg": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["msg"]
        }),
      },
    }->makeFields,
})
t_User.contents = GraphQLObjectType.make({
  name: "User",
  description: "A user in the system.",
  interfaces: [get_HasName(), get_Node()],
  fields: () =>
    {
      "abbreviatedName": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          HasNameInterfaceResolvers.abbreviatedName(src, ~typeName=User)
        }),
      },
      "age": {
        typ: Scalars.int->Scalars.toGraphQLType->nonNull,
        description: "The age of the user.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["age"]
        }),
      },
      "allNames": {
        typ: GraphQLListType.make(Scalars.string->Scalars.toGraphQLType->nonNull)
        ->GraphQLListType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.UserFields.allNames(src)
        }),
      },
      "currentStatus": {
        typ: enum_UserStatus->GraphQLEnumType.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.UserFields.currentStatus(src)
        }),
      },
      "id": {
        typ: Scalars.id->Scalars.toGraphQLType->nonNull,
        description: "The id of the object.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          NodeInterfaceResolver.id(src, ~typename=User)
        }),
      },
      "lastAge": {
        typ: Scalars.int->Scalars.toGraphQLType,
        description: "The last age of the user.",
        deprecationReason: "Use 'age' instead.",
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["lastAge"]
        }),
      },
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        args: {"includeFullName": {typ: Scalars.boolean->Scalars.toGraphQLType}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.UserFields.name(src, ~includeFullName=args["includeFullName"]->Nullable.toOption)
        }),
      },
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["name"]
        }),
      },
    }->makeFields,
})
t_UserConnection.contents = GraphQLObjectType.make({
  name: "UserConnection",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "edges": {
        typ: GraphQLListType.make(
          get_UserEdge()->GraphQLObjectType.toGraphQLType,
        )->GraphQLListType.toGraphQLType,
        description: "A list of edges.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["edges"]
        }),
      },
      "pageInfo": {
        typ: get_PageInfo()->GraphQLObjectType.toGraphQLType->nonNull,
        description: "Information to aid in pagination.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["pageInfo"]
        }),
      },
      "totalCount": {
        typ: Scalars.int->Scalars.toGraphQLType,
        description: "The total count of edges available in the connection.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          Schema.totalCount(src)
        }),
      },
    }->makeFields,
})
t_UserEdge.contents = GraphQLObjectType.make({
  name: "UserEdge",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "cursor": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: "A cursor for use in pagination.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["cursor"]
        }),
      },
      "node": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: "The item at the end of the edge.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["node"]
        }),
      },
    }->makeFields,
})
input_LocationByMagicString.contents = GraphQLInputObjectType.make({
  name: "LocationByMagicString",
  description: ?None,
  fields: () =>
    {
      "text": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
input_PaginationArgsBackwards.contents = GraphQLInputObjectType.make({
  name: "PaginationArgsBackwards",
  description: ?None,
  fields: () =>
    {
      "before": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "last": {
        GraphQLInputObjectType.typ: Scalars.int->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
input_PaginationArgsForward.contents = GraphQLInputObjectType.make({
  name: "PaginationArgsForward",
  description: ?None,
  fields: () =>
    {
      "after": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "first": {
        GraphQLInputObjectType.typ: Scalars.int->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
input_Address.contents = GraphQLInputObjectType.make({
  name: "Address",
  description: ?None,
  fields: () =>
    {
      "city": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
      "postalCode": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
      "streetAdddress": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
input_Coordinates.contents = GraphQLInputObjectType.make({
  name: "Coordinates",
  description: ?None,
  fields: () =>
    {
      "lat": {
        GraphQLInputObjectType.typ: Scalars.float->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
      "lon": {
        GraphQLInputObjectType.typ: Scalars.float->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
input_UserConfig.contents = GraphQLInputObjectType.make({
  name: "UserConfig",
  description: "Configuration for searching for a user.",
  fields: () =>
    {
      "context": {
        GraphQLInputObjectType.typ: get_UserConfigContext()->GraphQLInputObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
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
inputUnion_Location.contents = GraphQLInputObjectType.make({
  name: "Location",
  description: ?None,
  fields: () =>
    {
      "byAddress": {
        GraphQLInputObjectType.typ: get_Address()->GraphQLInputObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "byCoordinates": {
        GraphQLInputObjectType.typ: get_Coordinates()->GraphQLInputObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "byId": {
        GraphQLInputObjectType.typ: Scalars.id->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "byMagicString": {
        GraphQLInputObjectType.typ: get_LocationByMagicString()->GraphQLInputObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_PaginationArgs.contents = GraphQLInputObjectType.make({
  name: "PaginationArgs",
  description: ?None,
  fields: () =>
    {
      "backwards": {
        GraphQLInputObjectType.typ: get_PaginationArgsBackwards()->GraphQLInputObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "forward": {
        GraphQLInputObjectType.typ: get_PaginationArgsForward()->GraphQLInputObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
union_InferredUnion.contents = GraphQLUnionType.make({
  name: "InferredUnion",
  description: ?None,
  types: () => [get_SomeOtherType(), get_SomeType()],
  resolveType: GraphQLUnionType.makeResolveUnionTypeFn(union_InferredUnion_resolveType),
})
union_InferredUnionWithInferredConstructor.contents = GraphQLUnionType.make({
  name: "InferredUnionWithInferredConstructor",
  description: ?None,
  types: () => [get_InferredUnionWithInferredConstructorSomeInferredType(), get_SomeType()],
  resolveType: GraphQLUnionType.makeResolveUnionTypeFn(
    union_InferredUnionWithInferredConstructor_resolveType,
  ),
})
union_InlineUnion.contents = GraphQLUnionType.make({
  name: "InlineUnion",
  description: ?None,
  types: () => [get_InlineUnionNotOk(), get_InlineUnionOk(), get_User()],
  resolveType: GraphQLUnionType.makeResolveUnionTypeFn(union_InlineUnion_resolveType),
})
union_UserOrGroup.contents = GraphQLUnionType.make({
  name: "UserOrGroup",
  description: "A user or a group.",
  types: () => [get_Group(), get_User()],
  resolveType: GraphQLUnionType.makeResolveUnionTypeFn(union_UserOrGroup_resolveType),
})

let schema = GraphQLSchemaType.make({
  "query": get_Query(),
  "mutation": get_Mutation(),
  "types": [
    get_Query()->GraphQLObjectType.toGraphQLType,
    get_Pet()->GraphQLObjectType.toGraphQLType,
    get_InlineUnionOk()->GraphQLObjectType.toGraphQLType,
    get_Group()->GraphQLObjectType.toGraphQLType,
    get_SomeType()->GraphQLObjectType.toGraphQLType,
    get_InlineUnionNotOk()->GraphQLObjectType.toGraphQLType,
    get_PageInfo()->GraphQLObjectType.toGraphQLType,
    get_InferredUnionWithInferredConstructorSomeInferredType()->GraphQLObjectType.toGraphQLType,
    get_UserConnection()->GraphQLObjectType.toGraphQLType,
    get_SomeOtherType()->GraphQLObjectType.toGraphQLType,
    get_UserEdge()->GraphQLObjectType.toGraphQLType,
    get_User()->GraphQLObjectType.toGraphQLType,
    get_Mutation()->GraphQLObjectType.toGraphQLType,
    get_HasName()->GraphQLInterfaceType.toGraphQLType,
    get_Node()->GraphQLInterfaceType.toGraphQLType,
    get_InlineUnion()->GraphQLUnionType.toGraphQLType,
    get_UserOrGroup()->GraphQLUnionType.toGraphQLType,
    get_InferredUnionWithInferredConstructor()->GraphQLUnionType.toGraphQLType,
    get_InferredUnion()->GraphQLUnionType.toGraphQLType,
    get_PaginationArgs()->GraphQLInputObjectType.toGraphQLType,
    get_Location()->GraphQLInputObjectType.toGraphQLType,
    get_PaginationArgsForward()->GraphQLInputObjectType.toGraphQLType,
    get_Address()->GraphQLInputObjectType.toGraphQLType,
    get_PaginationArgsBackwards()->GraphQLInputObjectType.toGraphQLType,
    get_Coordinates()->GraphQLInputObjectType.toGraphQLType,
    get_UserConfigContext()->GraphQLInputObjectType.toGraphQLType,
    get_UserConfig()->GraphQLInputObjectType.toGraphQLType,
    get_LocationByMagicString()->GraphQLInputObjectType.toGraphQLType,
    enum_InferredEnum->GraphQLEnumType.toGraphQLType,
    enum_UserStatus->GraphQLEnumType.toGraphQLType,
    enum_InferredEnumAsArgStatus->GraphQLEnumType.toGraphQLType,
  ],
})
