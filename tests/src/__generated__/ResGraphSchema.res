@@warning("-27-32")

open ResGraph__GraphQLJs

let typeUnwrapper: ('src) => 'return = %raw(`function typeUnwrapper(src) { if (src == null) return null; if (typeof src === 'object' && src.hasOwnProperty('_0')) return src['_0']; if (typeof src === 'object' && src.hasOwnProperty('VAL')) return src['VAL']; return src;}`)
let inputUnionUnwrapper: ('src, array<string>, array<string>) => 'return = %raw(`function inputUnionUnwrapper(src, inlineRecordTypenames, emptyPayloadTypenames) {
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
let resolveInterfaceTypename: ('src, array<string>, string, string) => string = %raw(`function resolveInterfaceTypename(src, allowedTypenames, interfaceName, interfaceResolverTypeName) {
      if (allowedTypenames.length === 1) {
        return allowedTypenames[0];
      }

      if (src != null && typeof src === "object") {
        let tag = src.TAG;

        if (typeof tag === "string" && allowedTypenames.includes(tag)) {
          return tag;
        }

        if (typeof tag === "string") {
          throw new Error(
            "Panic! Interface " +
              interfaceName +
              " resolveType got unexpected TAG " +
              JSON.stringify(tag) +
              ". Expected one of " +
              allowedTypenames.join(", ") +
              ".",
          );
        }
      }

      throw new Error(
        "Panic! Interface " +
          interfaceName +
          " resolveType expected a tagged value from " +
          interfaceResolverTypeName +
          ", but got an untagged value. Use " +
          interfaceResolverTypeName +
          " for interface return values instead of the bare interface record type.",
      );
    }
    `)

type inputObjectFieldConverterFn
external makeInputObjectFieldConverterFn: ('a => 'b) => inputObjectFieldConverterFn = "%identity"

let applyConversionToInputObject: ('a, array<(string, inputObjectFieldConverterFn)>) => 'a = %raw(`function applyConversionToInputObject(obj, instructions) {
  if (instructions.length === 0) return obj;
  let newObj = Object.assign({}, obj);
  instructions.forEach(instruction => {
    let value = newObj[instruction[0]];
    newObj[instruction[0]] = instruction[1](value);
  })
  return newObj;
}`)

let scalar_LiteralText = GraphQLScalar.make({
  let config: GraphQLScalar.config<AppCustomScalars.LiteralText.t> = {
    name: "LiteralText",
    description: "Text scalar with explicit literal coercion.",
    specifiedByURL: ?(None),
    parseValue: AppCustomScalars.LiteralText.parseValue,
    parseLiteral: AppCustomScalars.LiteralText.parseLiteral,
    serialize: AppCustomScalars.LiteralText.serialize,
  }
  config
})
let scalar_Uuid = GraphQLScalar.make({name: "Uuid", description: "Custom scalar with specifiedByUrl coverage.", specifiedByURL: "https://example.com/specifiedBy/uuid", extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("scalar")}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("scalar")}}]}}})

let enum_DirectiveStatus = GraphQLEnumType.make({
  name: "DirectiveStatus",
  description: ?(None),
  extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("enum")}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("enum")}}]}},
  values: {
    "Active": {GraphQLEnumType.value: "Active", description: ?(None), deprecationReason: ?(None), extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("enum-value")}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("enum-value")}}]}}},
  }->makeEnumValues,
})

let i_CompanyHolder: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_CompanyHolder = () => i_CompanyHolder.contents
let i_ContextOverride: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_ContextOverride = () => i_ContextOverride.contents
let i_Contextual: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Contextual = () => i_Contextual.contents
let i_Defaultable: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Defaultable = () => i_Defaultable.contents
let i_Labelled: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Labelled = () => i_Labelled.contents
let i_Named: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Named = () => i_Named.contents
let i_NamedEntity: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_NamedEntity = () => i_NamedEntity.contents
let i_Node: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Node = () => i_Node.contents
let i_NullableNamed: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_NullableNamed = () => i_NullableNamed.contents
let i_Ranked: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Ranked = () => i_Ranked.contents
let i_Searchable: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": null})
let get_Searchable = () => i_Searchable.contents
let t_DescribedUnionPayload: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_DescribedUnionPayload = () => t_DescribedUnionPayload.contents
let t_DirectiveExample: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_DirectiveExample = () => t_DirectiveExample.contents
let t_ExplicitCompany: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ExplicitCompany = () => t_ExplicitCompany.contents
let t_ExplicitCompanyHolder: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ExplicitCompanyHolder = () => t_ExplicitCompanyHolder.contents
let t_ExplicitContextOverrideResult: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ExplicitContextOverrideResult = () => t_ExplicitContextOverrideResult.contents
let t_ExplicitContextResult: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ExplicitContextResult = () => t_ExplicitContextResult.contents
let t_ExplicitDefaultable: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ExplicitDefaultable = () => t_ExplicitDefaultable.contents
let t_ExplicitSearchResult: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ExplicitSearchResult = () => t_ExplicitSearchResult.contents
let t_FunctionFieldRegression: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_FunctionFieldRegression = () => t_FunctionFieldRegression.contents
let t_LabelledAlpha: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_LabelledAlpha = () => t_LabelledAlpha.contents
let t_LabelledBeta: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_LabelledBeta = () => t_LabelledBeta.contents
let t_LabelledWrapper: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_LabelledWrapper = () => t_LabelledWrapper.contents
let t_Mutation: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Mutation = () => t_Mutation.contents
let t_NullableInterop: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_NullableInterop = () => t_NullableInterop.contents
let t_PageInfo: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_PageInfo = () => t_PageInfo.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Query = () => t_Query.contents
let t_Res12Record: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Res12Record = () => t_Res12Record.contents
let t_ReservedWordRecord: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ReservedWordRecord = () => t_ReservedWordRecord.contents
let t_ScalarHolder: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_ScalarHolder = () => t_ScalarHolder.contents
let t_StringConnection: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_StringConnection = () => t_StringConnection.contents
let t_StringEdge: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_StringEdge = () => t_StringEdge.contents
let t_Subscription: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Subscription = () => t_Subscription.contents
let t_T: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_T = () => t_T.contents
let t_Thing: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_Thing = () => t_Thing.contents
let t_User: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_User = () => t_User.contents
let t_UserConnection: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_UserConnection = () => t_UserConnection.contents
let t_UserEdge: ref<GraphQLObjectType.t> = Obj.magic({"contents": null})
let get_UserEdge = () => t_UserEdge.contents
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
let inputUnion_UpdatableNullableString: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableNullableString = () => inputUnion_UpdatableNullableString.contents
let inputUnion_UpdatableNullableString_conversionInstructions = []
let inputUnion_UpdatableString: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdatableString = () => inputUnion_UpdatableString.contents
let inputUnion_UpdatableString_conversionInstructions = []
let input_Res12InputInline: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_Res12InputInline = () => input_Res12InputInline.contents
let input_Res12InputInline_conversionInstructions = []
let input_DirectiveInput: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_DirectiveInput = () => input_DirectiveInput.contents
let input_DirectiveInput_conversionInstructions = []
let input_ReservedWordInput: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_ReservedWordInput = () => input_ReservedWordInput.contents
let input_ReservedWordInput_conversionInstructions = []
let input_UpdateThingInput: ref<GraphQLInputObjectType.t> = Obj.magic({"contents": null})
let get_UpdateThingInput = () => input_UpdateThingInput.contents
let input_UpdateThingInput_conversionInstructions = []
input_Res12InputInline_conversionInstructions->Array.pushMany([])
input_DirectiveInput_conversionInstructions->Array.pushMany([])
input_ReservedWordInput_conversionInstructions->Array.pushMany([])
input_UpdateThingInput_conversionInstructions->Array.pushMany([
  (
    "name",
    makeInputObjectFieldConverterFn((v) => v->applyConversionToInputObject(inputUnion_UpdatableString_conversionInstructions)->inputUnionUnwrapper([], ["LeaveUnchanged"]))
  ),
  (
    "age",
    makeInputObjectFieldConverterFn((v) => v->applyConversionToInputObject(inputUnion_UpdatableInt_conversionInstructions)->inputUnionUnwrapper([], ["LeaveUnchanged"]))
  ),
  (
    "favoriteColor",
    makeInputObjectFieldConverterFn((v) => v->applyConversionToInputObject(inputUnion_UpdatableNullableString_conversionInstructions)->inputUnionUnwrapper([], ["UnsetValue", "LeaveUnchanged"]))
  ),
  (
    "isAdmin",
    makeInputObjectFieldConverterFn((v) => v->applyConversionToInputObject(inputUnion_UpdatableNullableBool_conversionInstructions)->inputUnionUnwrapper([], ["UnsetValue", "LeaveUnchanged"]))
  ),
  (
    "height",
    makeInputObjectFieldConverterFn((v) => v->applyConversionToInputObject(inputUnion_UpdatableNullableFloat_conversionInstructions)->inputUnionUnwrapper([], ["UnsetValue", "LeaveUnchanged"]))
  ),
])
inputUnion_Res12Input_conversionInstructions->Array.pushMany([
  (
    "inline",
    makeInputObjectFieldConverterFn((v) => (switch v->Nullable.toOption { | None => None | Some(v) => v->applyConversionToInputObject(input_Res12InputInline_conversionInstructions)->Some}))
  ),
  (
    "empty",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableBool_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableFloat_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableInt_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableNullableBool_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "unsetValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableNullableFloat_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "unsetValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableNullableInt_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "unsetValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableNullableString_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "unsetValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
inputUnion_UpdatableString_conversionInstructions->Array.pushMany([
  (
    "updateValue",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
  (
    "leaveUnchanged",
    makeInputObjectFieldConverterFn((v) => (v->Nullable.toOption))
  ),
])
let union_DescribedUnion: ref<GraphQLUnionType.t> = Obj.magic({"contents": null})
let get_DescribedUnion = () => union_DescribedUnion.contents

let union_DescribedUnion_resolveType = (v: AppDirectives.describedUnion) => switch v { | Described(_) => "DescribedUnionPayload"}

let interface_CompanyHolder_resolveType = (v: Interface_companyHolder.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitCompanyHolder"], "CompanyHolder", "Interface_companyHolder.Resolver.t")

let interface_ContextOverride_resolveType = (v: Interface_contextOverride.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitContextOverrideResult"], "ContextOverride", "Interface_contextOverride.Resolver.t")

let interface_Contextual_resolveType = (v: Interface_contextual.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitContextResult"], "Contextual", "Interface_contextual.Resolver.t")

let interface_Defaultable_resolveType = (v: Interface_defaultable.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitDefaultable"], "Defaultable", "Interface_defaultable.Resolver.t")

let interface_Labelled_resolveType = (v: Interface_labelled.Resolver.t) => resolveInterfaceTypename(v, ["LabelledAlpha", "LabelledBeta"], "Labelled", "Interface_labelled.Resolver.t")

let interface_Named_resolveType = (v: Interface_named.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitCompany"], "Named", "Interface_named.Resolver.t")

let interface_NamedEntity_resolveType = (v: Interface_namedEntity.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitCompany"], "NamedEntity", "Interface_namedEntity.Resolver.t")

let interface_Node_resolveType = (v: Interface_node.Resolver.t) => resolveInterfaceTypename(v, ["Thing"], "Node", "Interface_node.Resolver.t")

let interface_NullableNamed_resolveType = (v: Interface_nullableNamed.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitCompany"], "NullableNamed", "Interface_nullableNamed.Resolver.t")

let interface_Ranked_resolveType = (v: Interface_ranked.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitCompany"], "Ranked", "Interface_ranked.Resolver.t")

let interface_Searchable_resolveType = (v: Interface_searchable.Resolver.t) => resolveInterfaceTypename(v, ["ExplicitSearchResult"], "Searchable", "Interface_searchable.Resolver.t")

i_CompanyHolder.contents = GraphQLInterfaceType.make({
  name: "CompanyHolder",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "company": {
      typ: get_NamedEntity()->GraphQLInterfaceType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_CompanyHolder_resolveType)
})
i_ContextOverride.contents = GraphQLInterfaceType.make({
  name: "ContextOverride",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "contextOverrideLabel": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_ContextOverride_resolveType)
})
i_Contextual.contents = GraphQLInterfaceType.make({
  name: "Contextual",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "contextLabel": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Contextual_resolveType)
})
i_Defaultable.contents = GraphQLInterfaceType.make({
  name: "Defaultable",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "format": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Defaultable_resolveType)
})
i_Labelled.contents = GraphQLInterfaceType.make({
  name: "Labelled",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "typenameEcho": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Labelled_resolveType)
})
i_Named.contents = GraphQLInterfaceType.make({
  name: "Named",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "name": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Named_resolveType)
})
i_NamedEntity.contents = GraphQLInterfaceType.make({
  name: "NamedEntity",
  description: ?(None),
  interfaces: [get_Named()],
  fields: () => {
    "entityKind": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "name": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_NamedEntity_resolveType)
})
i_Node.contents = GraphQLInterfaceType.make({
  name: "Node",
  description: "An object with an ID",
  interfaces: [],
  fields: () => {
    "id": {
      typ: Scalars.id->Scalars.toGraphQLType->nonNull,
      description: "The id of the object.",
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Node_resolveType)
})
i_NullableNamed.contents = GraphQLInterfaceType.make({
  name: "NullableNamed",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "nullableName": {
      typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_NullableNamed_resolveType)
})
i_Ranked.contents = GraphQLInterfaceType.make({
  name: "Ranked",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "rank": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Ranked_resolveType)
})
i_Searchable.contents = GraphQLInterfaceType.make({
  name: "Searchable",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "label": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "prefix": ({typ: Scalars.string->Scalars.toGraphQLType}: arg)
      }->makeArgsDict,
    }
  }->makeFields,
  resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_Searchable_resolveType)
})
t_DescribedUnionPayload.contents = GraphQLObjectType.make({
  name: "DescribedUnionPayload",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "value": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["value"]})
    }
  }->makeFields
})
t_DirectiveExample.contents = GraphQLObjectType.make({
  name: "DirectiveExample",
  description: ?(None),
  interfaces: [],
  extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("first")}, dict{"name": GraphQLLiteralValue.String("second")}], "cacheControl": [dict{"maxAge": GraphQLLiteralValue.Number(30.)}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("first")}}, {name: "cacheControl", args: dict{"maxAge": GraphQLLiteralValue.Number(30.)}}, {name: "tag", args: dict{"name": GraphQLLiteralValue.String("second")}}]}},
  fields: () => {
    "status": {
      typ: enum_DirectiveStatus->GraphQLEnumType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["status"]})
    },
    "value": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      extensions: {directives: dict{"cacheControl": [dict{"maxAge": GraphQLLiteralValue.Number(10.), "scope": GraphQLLiteralValue.String("private")}], "tag": [dict{"name": GraphQLLiteralValue.String("field")}]}, resgraph: {appliedDirectives: [{name: "cacheControl", args: dict{"maxAge": GraphQLLiteralValue.Number(10.), "scope": GraphQLLiteralValue.String("private")}}, {name: "tag", args: dict{"name": GraphQLLiteralValue.String("field")}}]}},
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["value"]})
    }
  }->makeFields
})
t_ExplicitCompany.contents = GraphQLObjectType.make({
  name: "ExplicitCompany",
  description: ?(None),
  interfaces: [get_Named(), get_NamedEntity(), get_NullableNamed(), get_Ranked()],
  fields: () => {
    "entityKind": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["entityKind"]})
    },
    "headquarters": {
      typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["headquarters"]})
    },
    "name": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["name"]})
    },
    "nullableName": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["nullableName"]})
    },
    "rank": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["rank"]})
    }
  }->makeFields
})
t_ExplicitCompanyHolder.contents = GraphQLObjectType.make({
  name: "ExplicitCompanyHolder",
  description: ?(None),
  interfaces: [get_CompanyHolder()],
  fields: () => {
    "company": {
      typ: get_ExplicitCompany()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["company"]})
    }
  }->makeFields
})
t_ExplicitContextOverrideResult.contents = GraphQLObjectType.make({
  name: "ExplicitContextOverrideResult",
  description: ?(None),
  interfaces: [get_ContextOverride()],
  fields: () => {
    "contextOverrideLabel": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.contextOverrideLabel(src, ~ctx=ctx)})
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["id"]})
    }
  }->makeFields
})
t_ExplicitContextResult.contents = GraphQLObjectType.make({
  name: "ExplicitContextResult",
  description: ?(None),
  interfaces: [get_Contextual()],
  fields: () => {
    "contextLabel": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.contextLabel(src)})
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["id"]})
    }
  }->makeFields
})
t_ExplicitDefaultable.contents = GraphQLObjectType.make({
  name: "ExplicitDefaultable",
  description: ?(None),
  interfaces: [get_Defaultable()],
  fields: () => {
    "format": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "suffix": ({
          typ: Scalars.string->Scalars.toGraphQLType,
          defaultValue: GraphQLLiteralValue.String("!"),
        }: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.ExplicitDefaultableFields.format(src, ~suffix=?((args["suffix"]->Nullable.toOption)))})
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["id"]})
    }
  }->makeFields
})
t_ExplicitSearchResult.contents = GraphQLObjectType.make({
  name: "ExplicitSearchResult",
  description: ?(None),
  interfaces: [get_Searchable()],
  fields: () => {
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["id"]})
    },
    "label": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "prefix": ({typ: Scalars.string->Scalars.toGraphQLType}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.label(src, ~prefix=?((args["prefix"]->Nullable.toOption)))})
    }
  }->makeFields
})
t_FunctionFieldRegression.contents = GraphQLObjectType.make({
  name: "FunctionFieldRegression",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "computedLabel": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); FunctionFieldRegression.computedLabel(src)})
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["id"]})
    }
  }->makeFields
})
t_LabelledAlpha.contents = GraphQLObjectType.make({
  name: "LabelledAlpha",
  description: ?(None),
  interfaces: [get_Labelled()],
  fields: () => {
    "extra": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["extra"]})
    },
    "typenameEcho": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppInterfaceExtras.typenameEcho(src, ~typeName=LabelledAlpha)})
    }
  }->makeFields
})
t_LabelledBeta.contents = GraphQLObjectType.make({
  name: "LabelledBeta",
  description: ?(None),
  interfaces: [get_Labelled()],
  fields: () => {
    "count": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["count"]})
    },
    "typenameEcho": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppInterfaceExtras.typenameEcho(src, ~typeName=LabelledBeta)})
    }
  }->makeFields
})
t_LabelledWrapper.contents = GraphQLObjectType.make({
  name: "LabelledWrapper",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "nested": {
      typ: get_Labelled()->GraphQLInterfaceType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["nested"]})
    }
  }->makeFields
})
t_Mutation.contents = GraphQLObjectType.make({
  name: "Mutation",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "shorthandIncrement": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "value": ({typ: Scalars.int->Scalars.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((_src, args, ctx, info) => {AppRootShorthand.shorthandIncrement(~value=args["value"])})
    },
    "updateThing": {
      typ: get_Thing()->GraphQLObjectType.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "input": ({typ: get_UpdateThingInput()->GraphQLInputObjectType.toGraphQLType->nonNull}: arg),
        "thingId": ({typ: Scalars.id->Scalars.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); Thing.updateThing(src, ~input=args["input"]->applyConversionToInputObject(input_UpdateThingInput_conversionInstructions), ~thingId=args["thingId"])})
    }
  }->makeFields
})
t_NullableInterop.contents = GraphQLObjectType.make({
  name: "NullableInterop",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "nullCount": {
      typ: Scalars.int->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["nullCount"]})
    },
    "nullableName": {
      typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["nullableName"]})
    }
  }->makeFields
})
t_PageInfo.contents = GraphQLObjectType.make({
  name: "PageInfo",
  description: "Information about pagination in a connection.",
  interfaces: [],
  fields: () => {
    "endCursor": {
      typ: Scalars.string->Scalars.toGraphQLType,
      description: "When paginating forwards, the cursor to continue.",
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["endCursor"]})
    },
    "hasNextPage": {
      typ: Scalars.boolean->Scalars.toGraphQLType->nonNull,
      description: "When paginating forwards, are there more items?",
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["hasNextPage"]})
    },
    "hasPreviousPage": {
      typ: Scalars.boolean->Scalars.toGraphQLType->nonNull,
      description: "When paginating backwards, are there more items?",
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["hasPreviousPage"]})
    },
    "startCursor": {
      typ: Scalars.string->Scalars.toGraphQLType,
      description: "When paginating backwards, the cursor to continue.",
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["startCursor"]})
    }
  }->makeFields
})
t_Query.contents = GraphQLObjectType.make({
  name: "Query",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "asyncValues": {
      typ: GraphQLListType.make(Scalars.string->Scalars.toGraphQLType->nonNull)->GraphQLListType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppAsyncIterable.asyncValues(src)})
    },
    "badLabelled": {
      typ: get_Labelled()->GraphQLInterfaceType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppInterfaceReturnRegression.badLabelled(src)})
    },
    "brokenLabelledWrapper": {
      typ: get_LabelledWrapper()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppInterfaceReturnRegression.brokenLabelledWrapper(src)})
    },
    "directiveArgumentMetadata": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "limit": ({
          typ: Scalars.int->Scalars.toGraphQLType,
          defaultValue: GraphQLLiteralValue.Number(25.),
          description: "Maximum number of results.",
          deprecationReason: "Use pageSize instead.",
          extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("argument")}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("argument")}}]}}
        }: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppDirectives.directiveArgumentMetadata(src, ~limit=?((args["limit"]->Nullable.toOption)))})
    },
    "directiveExample": {
      typ: get_DirectiveExample()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "input": ({typ: get_DirectiveInput()->GraphQLInputObjectType.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppDirectives.directiveExample(src, ~input=args["input"]->applyConversionToInputObject(input_DirectiveInput_conversionInstructions))})
    },
    "directiveInputDefault": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "input": ({typ: get_DirectiveInput()->GraphQLInputObjectType.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppDirectives.directiveInputDefault(src, ~input=args["input"]->applyConversionToInputObject(input_DirectiveInput_conversionInstructions))})
    },
    "explicitCompany": {
      typ: get_ExplicitCompany()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.explicitCompany(src)})
    },
    "explicitCompanyHolder": {
      typ: get_ExplicitCompanyHolder()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.explicitCompanyHolder(src)})
    },
    "explicitContextOverrideResult": {
      typ: get_ExplicitContextOverrideResult()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.explicitContextOverrideResult(src)})
    },
    "explicitContextResult": {
      typ: get_ExplicitContextResult()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.explicitContextResult(src)})
    },
    "explicitNamed": {
      typ: get_Named()->GraphQLInterfaceType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceReturns.explicitNamed(src)})
    },
    "explicitSearchResult": {
      typ: get_ExplicitSearchResult()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceImplements.explicitSearchResult(src)})
    },
    "explicitSearchable": {
      typ: get_Searchable()->GraphQLInterfaceType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppExplicitInterfaceReturns.explicitSearchable(src)})
    },
    "functionFieldRegression": {
      typ: get_FunctionFieldRegression()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); FunctionFieldRegression.functionFieldRegression(src)})
    },
    "getLabelled": {
      typ: get_LabelledAlpha()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppLabelledTypes.getLabelled(src)})
    },
    "getScalarHolder": {
      typ: get_ScalarHolder()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppCustomScalars.getScalarHolder(src)})
    },
    "goodLabelled": {
      typ: get_Labelled()->GraphQLInterfaceType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppInterfaceReturnRegression.goodLabelled(src)})
    },
    "labelledWrapper": {
      typ: get_LabelledWrapper()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppInterfaceReturnRegression.labelledWrapper(src)})
    },
    "literalText": {
      typ: scalar_LiteralText->GraphQLScalar.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "value": ({typ: scalar_LiteralText->GraphQLScalar.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((_src, args, ctx, info) => {AppCustomScalars.literalText(~value=args["value"])})
    },
    "nestedConnection": {
      typ: get_StringConnection()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppConnections.nestedConnection(src)})
    },
    "node": {
      typ: get_Node()->GraphQLInterfaceType.toGraphQLType,
      description: "Fetches an object given its ID.",
      deprecationReason: ?(None),
      args: dict{
        "id": ({typ: Scalars.id->Scalars.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); NodeInterfaceResolver.node(src, ~ctx=ctx, ~id=args["id"])})
    },
    "nodes": {
      typ: GraphQLListType.make(get_Node()->GraphQLInterfaceType.toGraphQLType)->GraphQLListType.toGraphQLType->nonNull,
      description: "Fetches objects given their IDs.",
      deprecationReason: ?(None),
      args: dict{
        "ids": ({typ: GraphQLListType.make(Scalars.id->Scalars.toGraphQLType->nonNull)->GraphQLListType.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); NodeInterfaceResolver.nodes(src, ~ctx=ctx, ~ids=args["ids"])})
    },
    "nullableInterop": {
      typ: get_NullableInterop()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "nullCount": ({typ: Scalars.int->Scalars.toGraphQLType}: arg),
        "nullableName": ({typ: Scalars.string->Scalars.toGraphQLType}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppNullableInterop.nullableInterop(src, ~nullCount=args["nullCount"], ~nullableName=args["nullableName"])})
    },
    "reservedWordArgumentEcho": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "constraint": ({typ: Scalars.string->Scalars.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppReScript12.reservedWordArgumentEcho(src, ~\"constraint"=args["constraint"])})
    },
    "reservedWordInputEcho": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "input": ({typ: get_ReservedWordInput()->GraphQLInputObjectType.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppReScript12.reservedWordInputEcho(src, ~input=args["input"]->applyConversionToInputObject(input_ReservedWordInput_conversionInstructions))})
    },
    "reservedWordRecord": {
      typ: get_ReservedWordRecord()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppReScript12.reservedWordRecord(src)})
    },
    "shorthandContext": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((_src, args, ctx, info) => {AppRootShorthand.shorthandContext(~ctx=ctx)})
    },
    "shorthandEcho": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "message": ({typ: Scalars.string->Scalars.toGraphQLType->nonNull}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((_src, args, ctx, info) => {AppRootShorthand.shorthandEcho(~message=args["message"])})
    },
    "shorthandGreeting": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: "A root query without an unused source argument.",
      deprecationReason: ?(None),
      resolve: makeResolveFn((_src, args, ctx, info) => {AppRootShorthand.shorthandGreeting(())})
    },
    "signatureMetadata": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "value": ({
          typ: Scalars.string->Scalars.toGraphQLType->nonNull,
          description: "Metadata loaded from the implementation source.",
        }: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppSignatureMetadata.signatureMetadata(src, ~value=args["value"])})
    },
    "userConnection": {
      typ: get_UserConnection()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      args: dict{
        "after": ({typ: Scalars.string->Scalars.toGraphQLType}: arg),
        "first": ({typ: Scalars.int->Scalars.toGraphQLType}: arg)
      }->makeArgsDict,
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppConnections.userConnection(src, ~after=?((args["after"]->Nullable.toOption)), ~first=?((args["first"]->Nullable.toOption)))})
    },
    "userDefinedNullable": {
      typ: get_T()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppUserDefinedNullable.userDefinedNullable(src)})
    }
  }->makeFields
})
t_Res12Record.contents = GraphQLObjectType.make({
  name: "Res12Record",
  description: "Extra coverage for ReScript 12 CMT/attribute changes.",
  interfaces: [],
  fields: () => {
    "oldField": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: "Deprecated attribute should survive too.",
      deprecationReason: "old field",
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["oldField"]})
    },
    "withDoc": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: "Doc should survive on fields.",
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["withDoc"]})
    }
  }->makeFields
})
t_ReservedWordRecord.contents = GraphQLObjectType.make({
  name: "ReservedWordRecord",
  description: "Reserved ReScript field names can be exposed as GraphQL names.",
  interfaces: [],
  fields: () => {
    "constraint": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["constraint"]})
    },
    "external": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["external"]})
    },
    "include": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["include"]})
    },
    "let": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["let"]})
    },
    "module": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["module"]})
    },
    "open": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["open"]})
    },
    "switch": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["switch"]})
    },
    "type": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["type"]})
    }
  }->makeFields
})
t_ScalarHolder.contents = GraphQLObjectType.make({
  name: "ScalarHolder",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "id": {
      typ: scalar_Uuid->GraphQLScalar.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["id"]})
    }
  }->makeFields
})
t_StringConnection.contents = GraphQLObjectType.make({
  name: "StringConnection",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "edges": {
      typ: GraphQLListType.make(get_StringEdge()->GraphQLObjectType.toGraphQLType)->GraphQLListType.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["edges"]})
    },
    "pageInfo": {
      typ: get_PageInfo()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["pageInfo"]})
    }
  }->makeFields
})
t_StringEdge.contents = GraphQLObjectType.make({
  name: "StringEdge",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "cursor": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["cursor"]})
    },
    "node": {
      typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["node"]})
    }
  }->makeFields
})
t_Subscription.contents = GraphQLObjectType.make({
  name: "Subscription",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "latestMessage": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((v, _, _, _) => v),
      subscribe: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); AppSubscription.latestMessage(src, ~ctx=ctx)})
    },
    "shorthandLatest": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((v, _, _, _) => v),
      subscribe: makeResolveFn((_src, args, ctx, info) => {AppRootShorthand.shorthandLatest(())})
    }
  }->makeFields
})
t_T.contents = GraphQLObjectType.make({
  name: "T",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "value": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["value"]})
    }
  }->makeFields
})
t_Thing.contents = GraphQLObjectType.make({
  name: "Thing",
  description: ?(None),
  interfaces: [get_Node()],
  fields: () => {
    "age": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["age"]})
    },
    "favoriteColor": {
      typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["favoriteColor"]})
    },
    "height": {
      typ: Scalars.float->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["height"]})
    },
    "id": {
      typ: Scalars.id->Scalars.toGraphQLType->nonNull,
      description: "The id of the object.",
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, args, ctx, info) => {let src = typeUnwrapper(src); NodeInterfaceResolver.id(src, ~typename=Thing)})
    },
    "isAdmin": {
      typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["isAdmin"]})
    },
    "name": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["name"]})
    }
  }->makeFields
})
t_User.contents = GraphQLObjectType.make({
  name: "User",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "age": {
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["age"]})
    },
    "id": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["id"]})
    },
    "lastAge": {
      typ: Scalars.int->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["lastAge"]})
    },
    "name": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["name"]})
    }
  }->makeFields
})
t_UserConnection.contents = GraphQLObjectType.make({
  name: "UserConnection",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "edges": {
      typ: GraphQLListType.make(get_UserEdge()->GraphQLObjectType.toGraphQLType)->GraphQLListType.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["edges"]})
    },
    "pageInfo": {
      typ: get_PageInfo()->GraphQLObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["pageInfo"]})
    }
  }->makeFields
})
t_UserEdge.contents = GraphQLObjectType.make({
  name: "UserEdge",
  description: ?(None),
  interfaces: [],
  fields: () => {
    "cursor": {
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["cursor"]})
    },
    "node": {
      typ: get_User()->GraphQLObjectType.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
      resolve: makeResolveFn((src, _args, _ctx, _info) => {let src = typeUnwrapper(src); src["node"]})
    }
  }->makeFields
})
input_Res12InputInline.contents = GraphQLInputObjectType.make({
  name: "Res12InputInline",
  description: ?(None),
  fields: () => {
    "payload": {
      GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: "Field doc on inline record.",
      deprecationReason: ?(None),
    }
  }->makeFields
})
input_DirectiveInput.contents = GraphQLInputObjectType.make({
  name: "DirectiveInput",
  description: ?(None),
  fields: () => {
    "label": {
      GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      defaultValue: GraphQLLiteralValue.String("fallback"),
      deprecationReason: ?(None),
    },
    "value": {
      GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
      extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("input-field")}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("input-field")}}]}}
    }
  }->makeFields,
  extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("input")}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("input")}}]}}
})
input_ReservedWordInput.contents = GraphQLInputObjectType.make({
  name: "ReservedWordInput",
  description: ?(None),
  fields: () => {
    "constraint": {
      GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "type": {
      GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields
})
input_UpdateThingInput.contents = GraphQLInputObjectType.make({
  name: "UpdateThingInput",
  description: ?(None),
  fields: () => {
    "age": {
      GraphQLInputObjectType.typ: get_UpdatableInt()->GraphQLInputObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "favoriteColor": {
      GraphQLInputObjectType.typ: get_UpdatableNullableString()->GraphQLInputObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "height": {
      GraphQLInputObjectType.typ: get_UpdatableNullableFloat()->GraphQLInputObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "isAdmin": {
      GraphQLInputObjectType.typ: get_UpdatableNullableBool()->GraphQLInputObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "name": {
      GraphQLInputObjectType.typ: get_UpdatableString()->GraphQLInputObjectType.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields
})
inputUnion_Res12Input.contents = GraphQLInputObjectType.make({
  name: "Res12Input",
  description: ?(None),
  fields: () => {
    "empty": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "inline": {
      GraphQLInputObjectType.typ: get_Res12InputInline()->GraphQLInputObjectType.toGraphQLType,
      description: " Inline record doc is preserved. ",
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableBool.contents = GraphQLInputObjectType.make({
  name: "UpdatableBool",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableFloat.contents = GraphQLInputObjectType.make({
  name: "UpdatableFloat",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.float->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableInt.contents = GraphQLInputObjectType.make({
  name: "UpdatableInt",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.int->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableNullableBool.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableBool",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "unsetValue": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableNullableFloat.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableFloat",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "unsetValue": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.float->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableNullableInt.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableInt",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "unsetValue": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.int->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableNullableString.contents = GraphQLInputObjectType.make({
  name: "UpdatableNullableString",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "unsetValue": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
inputUnion_UpdatableString.contents = GraphQLInputObjectType.make({
  name: "UpdatableString",
  description: ?(None),
  fields: () => {
    "leaveUnchanged": {
      GraphQLInputObjectType.typ: Scalars.boolean->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    },
    "updateValue": {
      GraphQLInputObjectType.typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }
  }->makeFields,
  isOneOf: true

})
union_DescribedUnion.contents = GraphQLUnionType.make({
  name: "DescribedUnion",
  description: ?(None),
  types: () => [get_DescribedUnionPayload()],
  resolveType: GraphQLUnionType.makeResolveUnionTypeFn(union_DescribedUnion_resolveType)
})

let directive_cacheControl = GraphQLDirective.make({
  name: "cacheControl",
  description: "Caching metadata consumed by a \u0022\u0022\u0022schema transform\u0022\u0022\u0022.",
  locations: ["OBJECT", "FIELD_DEFINITION"],
  args: dict{
    "maxAge": ({
      typ: Scalars.int->Scalars.toGraphQLType->nonNull,
      defaultValue: GraphQLLiteralValue.Number(60.),
      description: "Maximum cache lifetime in seconds.",
      deprecationReason: ?(None),
    }: arg),
    "scope": ({
      typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: ?(None),
    }: arg),
    "legacyScope": ({
      typ: Scalars.string->Scalars.toGraphQLType,
      description: ?(None),
      deprecationReason: "Use \"scope\" instead.",
    }: arg)
  }->makeArgsDict,
  isRepeatable: false
})
let directive_tag = GraphQLDirective.make({
  name: "tag",
  description: "Repeatable labels for schema elements.",
  locations: ["SCHEMA", "SCALAR", "OBJECT", "FIELD_DEFINITION", "ARGUMENT_DEFINITION", "ENUM", "ENUM_VALUE", "INPUT_OBJECT", "INPUT_FIELD_DEFINITION"],
  args: dict{
    "name": ({
      typ: Scalars.string->Scalars.toGraphQLType->nonNull,
      description: ?(None),
      deprecationReason: ?(None),
    }: arg)
  }->makeArgsDict,
  isRepeatable: true
})

let schema = GraphQLSchemaType.makeConfig({
  description: "The public \u0022\u0022\u0022ResGraph\u0022\u0022\u0022 test schema.",
  query: get_Query(),
  mutation: get_Mutation(),
  subscription: get_Subscription(),
  directives: [...GraphQLDirective.specifiedDirectives, directive_cacheControl, directive_tag],
  extensions: {directives: dict{"tag": [dict{"name": GraphQLLiteralValue.String("schema")}]}, resgraph: {appliedDirectives: [{name: "tag", args: dict{"name": GraphQLLiteralValue.String("schema")}}]}},
  types: [
    get_DescribedUnionPayload()->GraphQLObjectType.toGraphQLType,
    get_DirectiveExample()->GraphQLObjectType.toGraphQLType,
    get_ExplicitCompany()->GraphQLObjectType.toGraphQLType,
    get_ExplicitCompanyHolder()->GraphQLObjectType.toGraphQLType,
    get_ExplicitContextOverrideResult()->GraphQLObjectType.toGraphQLType,
    get_ExplicitContextResult()->GraphQLObjectType.toGraphQLType,
    get_ExplicitDefaultable()->GraphQLObjectType.toGraphQLType,
    get_ExplicitSearchResult()->GraphQLObjectType.toGraphQLType,
    get_FunctionFieldRegression()->GraphQLObjectType.toGraphQLType,
    get_LabelledAlpha()->GraphQLObjectType.toGraphQLType,
    get_LabelledBeta()->GraphQLObjectType.toGraphQLType,
    get_LabelledWrapper()->GraphQLObjectType.toGraphQLType,
    get_Mutation()->GraphQLObjectType.toGraphQLType,
    get_NullableInterop()->GraphQLObjectType.toGraphQLType,
    get_PageInfo()->GraphQLObjectType.toGraphQLType,
    get_Query()->GraphQLObjectType.toGraphQLType,
    get_Res12Record()->GraphQLObjectType.toGraphQLType,
    get_ReservedWordRecord()->GraphQLObjectType.toGraphQLType,
    get_ScalarHolder()->GraphQLObjectType.toGraphQLType,
    get_StringConnection()->GraphQLObjectType.toGraphQLType,
    get_StringEdge()->GraphQLObjectType.toGraphQLType,
    get_Subscription()->GraphQLObjectType.toGraphQLType,
    get_T()->GraphQLObjectType.toGraphQLType,
    get_Thing()->GraphQLObjectType.toGraphQLType,
    get_User()->GraphQLObjectType.toGraphQLType,
    get_UserConnection()->GraphQLObjectType.toGraphQLType,
    get_UserEdge()->GraphQLObjectType.toGraphQLType,
    get_CompanyHolder()->GraphQLInterfaceType.toGraphQLType,
    get_ContextOverride()->GraphQLInterfaceType.toGraphQLType,
    get_Contextual()->GraphQLInterfaceType.toGraphQLType,
    get_Defaultable()->GraphQLInterfaceType.toGraphQLType,
    get_Labelled()->GraphQLInterfaceType.toGraphQLType,
    get_Named()->GraphQLInterfaceType.toGraphQLType,
    get_NamedEntity()->GraphQLInterfaceType.toGraphQLType,
    get_Node()->GraphQLInterfaceType.toGraphQLType,
    get_NullableNamed()->GraphQLInterfaceType.toGraphQLType,
    get_Ranked()->GraphQLInterfaceType.toGraphQLType,
    get_Searchable()->GraphQLInterfaceType.toGraphQLType,
    get_DescribedUnion()->GraphQLUnionType.toGraphQLType,
    get_Res12Input()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableBool()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableFloat()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableInt()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableNullableBool()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableNullableFloat()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableNullableInt()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableNullableString()->GraphQLInputObjectType.toGraphQLType,
    get_UpdatableString()->GraphQLInputObjectType.toGraphQLType,
    get_Res12InputInline()->GraphQLInputObjectType.toGraphQLType,
    get_DirectiveInput()->GraphQLInputObjectType.toGraphQLType,
    get_ReservedWordInput()->GraphQLInputObjectType.toGraphQLType,
    get_UpdateThingInput()->GraphQLInputObjectType.toGraphQLType,
    enum_DirectiveStatus->GraphQLEnumType.toGraphQLType
  ]
})
