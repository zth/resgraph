@@warning("-27-32")

open ResGraph__GraphQLJs

let typeUnwrapper: 'src => 'return = %raw(`function typeUnwrapper(src) { if (src == null) return null; if (typeof src === 'object' && src.hasOwnProperty('_0')) return src['_0']; return src;}`)
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

let scalar_Timestamp = GraphQLScalar.make({name: "Timestamp", description: ?None})
let scalar_Timestamp2 = GraphQLScalar.make({name: "Timestamp2", description: ?None})
let enum_TimestampFormat = GraphQLEnumType.make({
  name: "TimestampFormat",
  description: ?None,
  values: {
    "Timestamp": {GraphQLEnumType.value: "Timestamp", description: ?None, deprecationReason: ?None},
    "HumanReadable": {
      GraphQLEnumType.value: "HumanReadable",
      description: ?None,
      deprecationReason: ?None,
    },
  }->makeEnumValues,
})
let i_HasName: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": Js.null})
let get_HasName = () => i_HasName.contents
let t_PageInfo: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_PageInfo = () => t_PageInfo.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Query = () => t_Query.contents
let t_User: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_User = () => t_User.contents
let input_FindShopInputByAddress: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_FindShopInputByAddress = () => input_FindShopInputByAddress.contents
let input_FindShopInputByAddress_conversionInstructions = []
let input_Coordinates: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_Coordinates = () => input_Coordinates.contents
let input_Coordinates_conversionInstructions = []
input_FindShopInputByAddress_conversionInstructions->Array.pushMany([])
input_Coordinates_conversionInstructions->Array.pushMany([])
let inputUnion_FindShopInput: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": Js.null})
let get_FindShopInput = () => inputUnion_FindShopInput.contents
let inputUnion_FindShopInput_conversionInstructions = []
inputUnion_FindShopInput_conversionInstructions->Array.pushMany([
  ("byId", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  (
    "byAddress",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) =>
        v->applyConversionToInputObject(input_FindShopInputByAddress_conversionInstructions)->Some
      }
    ),
  ),
  (
    "byCoordinates",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) => v->applyConversionToInputObject(input_Coordinates_conversionInstructions)->Some
      }
    ),
  ),
])

let interface_HasName_resolveType = (v: Interface_hasName.Resolver.t) =>
  switch v {
  | User(_) => "User"
  }

i_HasName.contents = GraphQLInterfaceType.make({
  name: "HasName",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "name": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_HasName_resolveType),
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
t_Query.contents = GraphQLObjectType.make({
  name: "Query",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "currentTime": {
        typ: scalar_Timestamp2->GraphQLScalar.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          GraphQLSchema.currentTime(src)
        }),
      },
      "currentTimeFloat": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {"format": {typ: enum_TimestampFormat->GraphQLEnumType.toGraphQLType}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          GraphQLSchema.currentTimeFloat(src, ~format=?args["format"]->Nullable.toOption)
        }),
      },
      "me": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {"onlyIfAvailable": {typ: Scalars.boolean->Scalars.toGraphQLType->nonNull}}->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          GraphQLSchema.me(src, ~onlyIfAvailable=args["onlyIfAvailable"])
        }),
      },
      "shop": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        args: {
          "input": {typ: get_FindShopInput()->GraphQLInputObjectType.toGraphQLType->nonNull},
        }->makeArgs,
        resolve: makeResolveFn((src, args, ctx, info) => {
          let src = typeUnwrapper(src)
          GraphQLSchema.shop(
            src,
            ~input=args["input"]
            ->applyConversionToInputObject(inputUnion_FindShopInput_conversionInstructions)
            ->inputUnionUnwrapper(["ByAddress"]),
          )
        }),
      },
    }->makeFields,
})
t_User.contents = GraphQLObjectType.make({
  name: "User",
  description: ?None,
  interfaces: [get_HasName()],
  fields: () =>
    {
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
input_FindShopInputByAddress.contents = GraphQLInputObjectType.make({
  name: "FindShopInputByAddress",
  description: ?None,
  fields: () =>
    {
      "postalCode": {
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
      "lng": {
        GraphQLInputObjectType.typ: Scalars.float->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
inputUnion_FindShopInput.contents = GraphQLInputObjectType.make({
  name: "FindShopInput",
  description: ?None,
  fields: () =>
    {
      "byAddress": {
        GraphQLInputObjectType.typ: get_FindShopInputByAddress()->GraphQLInputObjectType.toGraphQLType,
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
    }->makeFields,
  extensions: {oneOf: true},
})

let schema = GraphQLSchemaType.make({"query": get_Query()})
