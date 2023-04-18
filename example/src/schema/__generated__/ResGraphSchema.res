@@warning("-27-32")

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

let i_HasName: ref<GraphQLInterfaceType.t> = Obj.magic({"contents": Js.null})
let get_HasName = () => i_HasName.contents
let t_User: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_User = () => t_User.contents
let t_Query: ref<GraphQLObjectType.t> = Obj.magic({"contents": Js.null})
let get_Query = () => t_Query.contents

let interface_HasName_resolveType = (v: ResGraphSchemaAssets.hasName_resolver) =>
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
  astNode: {
    uri: "/Users/zth/git/resgraph/example/src/schema/HasNameInterface.res",
    range: {"start": {"line": 1, "character": 0}, "end": {"line": 1, "character": 40}},
  },
})
t_User.contents = GraphQLObjectType.make({
  name: "User",
  description: ?None,
  interfaces: [get_HasName()],
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
      "age": {
        typ: Scalars.int->Scalars.toGraphQLType->nonNull,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, _args, _ctx) => {
          let src = typeUnwrapper(src)
          src["age"]
        }),
      },
    }->makeFields,
  astNode: {
    uri: "/Users/zth/git/resgraph/example/src/schema/GraphQLSchema.res",
    range: {"start": {"line": 4, "character": 0}, "end": {"line": 4, "character": 62}},
  },
})
t_Query.contents = GraphQLObjectType.make({
  name: "Query",
  description: ?None,
  interfaces: [],
  fields: () =>
    {
      "me": {
        typ: get_User()->GraphQLObjectType.toGraphQLType,
        description: ?None,
        deprecationReason: ?None,
        resolve: makeResolveFn((src, args, ctx) => {
          let src = typeUnwrapper(src)
          GraphQLSchema.me(src)
        }),
      },
    }->makeFields,
  astNode: {
    uri: "/Users/zth/git/resgraph/example/src/schema/GraphQLSchema.res",
    range: {"start": {"line": 1, "character": 0}, "end": {"line": 1, "character": 15}},
  },
})

let schema = GraphQLSchemaType.make({"query": get_Query()})
