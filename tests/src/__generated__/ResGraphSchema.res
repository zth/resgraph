@@warning("-27-32")

open ResGraph__GraphQLJs

let typeUnwrapper: 'src => 'return = %raw(`function typeUnwrapper(src) { if (src == null) return null; if (typeof src === 'object' && src.hasOwnProperty('_0')) return src['_0']; if (typeof src === 'object' && src.hasOwnProperty('VAL')) return src['VAL']; return src;}`)
let inputUnionUnwrapper: (
  'src,
  array<string>,
  array<string>,
) => 'return = %raw(`function inputUnionUnwrapper(src, inlineRecordTypenames, emptyPayloadTypenames) {
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

        if (emptyPayloadTypenames.includes(tagName)) {
          return tagName;
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

let scalar_Uuid = GraphQLScalar.make({
  name: "Uuid",
  description: "Custom scalar with specifiedByUrl coverage.",
})
let i_Labelled: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Labelled = () => i_Labelled.contents
let i_Node: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Node = () => i_Node.contents
let t_LabelledAlpha: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_LabelledAlpha = () => t_LabelledAlpha.contents
let t_LabelledBeta: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_LabelledBeta = () => t_LabelledBeta.contents
let t_Mutation: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Mutation = () => t_Mutation.contents
let t_PageInfo: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_PageInfo = () => t_PageInfo.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Query = () => t_Query.contents
let t_Res12Record: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Res12Record = () => t_Res12Record.contents
let t_ScalarHolder: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ScalarHolder = () => t_ScalarHolder.contents
let t_Subscription: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Subscription = () => t_Subscription.contents
let t_Thing: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Thing = () => t_Thing.contents
let inputUnion_Res12Input: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_Res12Input = () => inputUnion_Res12Input.contents
let inputUnion_Res12Input_conversionInstructions = []
let inputUnion_UpdatableBool: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableBool = () => inputUnion_UpdatableBool.contents
let inputUnion_UpdatableBool_conversionInstructions = []
let inputUnion_UpdatableFloat: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableFloat = () => inputUnion_UpdatableFloat.contents
let inputUnion_UpdatableFloat_conversionInstructions = []
let inputUnion_UpdatableInt: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableInt = () => inputUnion_UpdatableInt.contents
let inputUnion_UpdatableInt_conversionInstructions = []
let inputUnion_UpdatableNullableBool: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableNullableBool = () => inputUnion_UpdatableNullableBool.contents
let inputUnion_UpdatableNullableBool_conversionInstructions = []
let inputUnion_UpdatableNullableFloat: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableNullableFloat = () => inputUnion_UpdatableNullableFloat.contents
let inputUnion_UpdatableNullableFloat_conversionInstructions = []
let inputUnion_UpdatableNullableInt: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableNullableInt = () => inputUnion_UpdatableNullableInt.contents
let inputUnion_UpdatableNullableInt_conversionInstructions = []
let inputUnion_UpdatableNullableString: ref<GraphQLInputObjectType.t> = Obj.magic({
  "contents": null,
})
let get_UpdatableNullableString = () => inputUnion_UpdatableNullableString.contents
let inputUnion_UpdatableNullableString_conversionInstructions = []
let inputUnion_UpdatableString: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableString = () => inputUnion_UpdatableString.contents
let inputUnion_UpdatableString_conversionInstructions = []
let input_Res12InputInline: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_Res12InputInline = () => input_Res12InputInline.contents
let input_Res12InputInline_conversionInstructions = []
let input_UpdateThingInput: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdateThingInput = () => input_UpdateThingInput.contents
let input_UpdateThingInput_conversionInstructions = []
input_Res12InputInline_conversionInstructions->Array.pushMany([])
input_UpdateThingInput_conversionInstructions->Array.pushMany([
  (
    "name",
    makeInputObjectFieldConverterFn(v =>
      v
      ->applyConversionToInputObject(inputUnion_UpdatableString_conversionInstructions)
      ->inputUnionUnwrapper([], ["LeaveUnchanged"])
    ),
  ),
  (
    "age",
    makeInputObjectFieldConverterFn(v =>
      v
      ->applyConversionToInputObject(inputUnion_UpdatableInt_conversionInstructions)
      ->inputUnionUnwrapper([], ["LeaveUnchanged"])
    ),
  ),
  (
    "favoriteColor",
    makeInputObjectFieldConverterFn(v =>
      v
      ->applyConversionToInputObject(inputUnion_UpdatableNullableString_conversionInstructions)
      ->inputUnionUnwrapper([], ["UnsetValue", "LeaveUnchanged"])
    ),
  ),
  (
    "isAdmin",
    makeInputObjectFieldConverterFn(v =>
      v
      ->applyConversionToInputObject(inputUnion_UpdatableNullableBool_conversionInstructions)
      ->inputUnionUnwrapper([], ["UnsetValue", "LeaveUnchanged"])
    ),
  ),
  (
    "height",
    makeInputObjectFieldConverterFn(v =>
      v
      ->applyConversionToInputObject(inputUnion_UpdatableNullableFloat_conversionInstructions)
      ->inputUnionUnwrapper([], ["UnsetValue", "LeaveUnchanged"])
    ),
  ),
])
inputUnion_Res12Input_conversionInstructions->Array.pushMany([
  (
    "inline",
    makeInputObjectFieldConverterFn(v =>
      switch v->Nullable.toOption {
      | None => None
      | Some(v) =>
        v->applyConversionToInputObject(input_Res12InputInline_conversionInstructions)->Some
      }
    ),
  ),
  ("empty", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableBool_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableFloat_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableInt_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableNullableBool_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("unsetValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableNullableFloat_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("unsetValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableNullableInt_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("unsetValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableNullableString_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("unsetValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])
inputUnion_UpdatableString_conversionInstructions->Array.pushMany([
  ("updateValue", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
  ("leaveUnchanged", makeInputObjectFieldConverterFn(v => v->Nullable.toOption)),
])

let interface_Labelled_resolveType = (v: Interface_labelled.Resolver.t) =>
  switch v {
  | LabelledBeta(_) => "LabelledBeta"
  | LabelledAlpha(_) => "LabelledAlpha"
  }

let interface_Node_resolveType = (v: Interface_node.Resolver.t) =>
  switch v {
  | Thing(_) => "Thing"
  }

i_Labelled.contents = GraphQLInterfaceType.make({
  name: "Labelled",
  description: ?None,
  interfaces: [],
  fields: () => {%raw(`{}`)}->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Labelled_resolveType),
})
i_Node.contents = GraphQLInterfaceType.make({
  name: "Node",
  description: "An object with an ID",
  interfaces: [],
  fields: () => {%raw(`{}`)}->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Node_resolveType),
})
t_LabelledAlpha.contents = GraphQLObjectType.make({
  name: "LabelledAlpha",
  description: ?None,
  interfaces: [get_Labelled()],
  fields: () =>
    {
      "extra": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["extra"]
        }),
      },
    }->makeFields,
})
t_LabelledBeta.contents = GraphQLObjectType.make({
  name: "LabelledBeta",
  description: ?None,
  interfaces: [get_Labelled()],
  fields: () =>
    {
      "count": {
        typ: Scalars.int->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["count"]
        }),
      },
    }->makeFields,
})
t_Mutation.contents = GraphQLObjectType.make({
  name: "Mutation",
  description: ?None,
  interfaces: [],
  fields: () => {%raw(`{}`)}->makeFields,
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
  fields: () => {%raw(`{}`)}->makeFields,
})
t_Res12Record.contents = GraphQLObjectType.make({
  name: "Res12Record",
  description: "Extra coverage for ReScript 12 CMT/attribute changes.",
  interfaces: [],
  fields: () =>
    {
      "oldField": {
        typ: Scalars.int->Scalars.toGraphQLType->nonNull,
        description: "Deprecated attribute should survive too.",
        deprecationReason: "old field",
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["oldField"]
        }),
      },
      "withDoc": {
        typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: "Doc should survive on fields.",
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["withDoc"]
        }),
      },
    }->makeFields,
})
t_ScalarHolder.contents = GraphQLObjectType.make({
  name: "ScalarHolder",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "id": {
        typ: scalar_Uuid->GraphQLScalar.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["id"]
        }),
      },
    }->makeFields,
})
t_Subscription.contents = GraphQLObjectType.make({
  name: "Subscription",
  description: ?None,
  interfaces: [],
  fields: () => {%raw(`{}`)}->makeFields,
})
t_Thing.contents = GraphQLObjectType.make({
  name: "Thing",
  description: ?None,
  interfaces: [get_Node()],
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
      "favoriteColor": {
        typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["favoriteColor"]
        }),
      },
      "height": {
        typ: Scalars.float->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["height"]
        }),
      },
      "isAdmin": {
        typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx, _info) => {
          let src = typeUnwrapper(src)
          src["isAdmin"]
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
input_Res12InputInline.contents = GraphQLInputObjectType.make({
  name: "Res12InputInline",
  description: ?None,
  fields: () =>
    {
      "payload": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
        description: "Field doc on inline record.",
        deprecationReason: ?None,
      },
    }->makeFields,
})
input_UpdateThingInput.contents = GraphQLInputObjectType.make({
  name: "UpdateThingInput",
  description: ?None,
  fields: () =>
    {
      "age": {
        GraphQLInputObjectType.typ: get_UpdatableInt()
        ->GraphQLInputObjectType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
      "favoriteColor": {
        GraphQLInputObjectType.typ: get_UpdatableNullableString()
        ->GraphQLInputObjectType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
      "height": {
        GraphQLInputObjectType.typ: get_UpdatableNullableFloat()
        ->GraphQLInputObjectType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
      "isAdmin": {
        GraphQLInputObjectType.typ: get_UpdatableNullableBool()
        ->GraphQLInputObjectType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
      "name": {
        GraphQLInputObjectType.typ: get_UpdatableString()
        ->GraphQLInputObjectType.toGraphQLType
        ->nonNull,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
})
inputUnion_Res12Input.contents = GraphQLInputObjectType.make({
  name: "Res12Input",
  description: ?None,
  fields: () =>
    {
      "empty": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "inline": {
        GraphQLInputObjectType.typ: get_Res12InputInline()->GraphQLInputObjectType.toGraphQLType,
        description: " Inline record doc is preserved. ",
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableBool.contents = GraphQLInputObjectType.make({
  name: "UpdatableBool",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableFloat.contents = GraphQLInputObjectType.make({
  name: "UpdatableFloat",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.float->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableInt.contents = GraphQLInputObjectType.make({
  name: "UpdatableInt",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.int->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableNullableBool.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableBool",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "unsetValue": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableNullableFloat.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableFloat",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "unsetValue": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.float->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableNullableInt.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableInt",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "unsetValue": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.int->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableNullableString.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableString",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "unsetValue": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})
inputUnion_UpdatableString.contents = GraphQLInputObjectType.make({
  name: "UpdatableString",
  description: ?None,
  fields: () =>
    {
      "leaveUnchanged": {
        GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
      "updateValue": {
        GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
      },
    }->makeFields,
  extensions: {oneOf: true},
})

let schema = GraphQLSchemaType.make({
  "query": get_Query(),
  "mutation": get_Mutation(),
  "subscription": get_Subscription(),
  "types": [
    get_Query()->GraphQLObjectType.toGraphQLType,
    get_LabelledBeta()->GraphQLObjectType.toGraphQLType,
    get_ScalarHolder()->GraphQLObjectType.toGraphQLType,
    get_PageInfo()->GraphQLObjectType.toGraphQLType,
    get_Subscription()->GraphQLObjectType.toGraphQLType,
    get_LabelledAlpha()->GraphQLObjectType.toGraphQLType,
    get_Mutation()->GraphQLObjectType.toGraphQLType,
    get_Res12Record()->GraphQLObjectType.toGraphQLType,
    get_Thing()->GraphQLObjectType.toGraphQLType,
    get_Labelled()->GraphQLInterfaceType.toGraphQLType,
    get_Node()->GraphQLInterfaceType.toGraphQLType,
    get_UpdatableNullableFloat()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableString()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableNullableBool()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableBool()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableFloat()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableNullableInt()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableInt()->GraphQLInputObjectType.toGraphQLType,
    get_Res12Input()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableNullableString()->GraphQLInputObjectType.toGraphQLType,
    get_UpdateThingInput()->GraphQLInputObjectType.toGraphQLType,
    get_Res12InputInline()->GraphQLInputObjectType.toGraphQLType,
  ],
})
