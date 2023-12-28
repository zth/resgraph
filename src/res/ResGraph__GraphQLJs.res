// TODO: This should be switched to JSON.t when unboxed variant representation
// has landed in Core
module GraphQLLiteralValue = {
  @unboxed
  type rec t =
    | @as(false) False
    | @as(true) True
    | @as(null) Null
    | String(string)
    | Number(float)
    | Object(Js.Dict.t<t>)
    | Array(array<t>)
}

type graphqlType

@module("graphql") @new external nonNull: graphqlType => graphqlType = "GraphQLNonNull"

module AstNode = {
  // Copy of LSP protocol stuff
  type loc = {"line": int, "character": int}

  type range = {"start": loc, "end": loc}

  type t = {
    uri: string,
    range: range,
  }
}

module Scalars = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  @module("graphql") @val external string: t = "GraphQLString"
  @module("graphql") @val external int: t = "GraphQLInt"
  @module("graphql") @val external id: t = "GraphQLID"
  @module("graphql") @val external float: t = "GraphQLFloat"
  @module("graphql") @val external boolean: t = "GraphQLBoolean"
}

type arg = {@as("type") typ: graphqlType}

type resolveFn

external makeResolveFn: (('srcValue, 'args, 'ctx, 'info) => 'return) => resolveFn = "%identity"

type fields

external makeFields: {..} => fields = "%identity"

type args

external makeArgs: {..} => args = "%identity"

type typeField = {
  @as("type") typ: graphqlType,
  args?: args,
  resolve?: resolveFn,
  description?: string,
  deprecationReason?: string,
}

module GraphQLInterfaceType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type resolveInterfaceTypeFn

  external makeResolveInterfaceTypeFn: ('source => 'return) => resolveInterfaceTypeFn = "%identity"

  type config = {
    name: string,
    astNode?: AstNode.t,
    description?: string,
    fields: unit => fields,
    resolveType: resolveInterfaceTypeFn,
    interfaces?: array<t>,
  }
  @module("graphql") @new external make: config => t = "GraphQLInterfaceType"
}

module GraphQLScalar = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type config<'t> = {
    name: string,
    description?: string,
    parseValue?: GraphQLLiteralValue.t => option<'t>,
    serialize?: 't => GraphQLLiteralValue.t,
  }
  @module("graphql") @new external make: config<_> => t = "GraphQLScalarType"
}

module GraphQLObjectType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type config = {
    name: string,
    astNode?: AstNode.t,
    description?: string,
    fields: unit => fields,
    interfaces?: array<GraphQLInterfaceType.t>,
  }
  @module("graphql") @new external make: config => t = "GraphQLObjectType"
}

module GraphQLInputObjectType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type inputObjectField = {
    @as("type") typ: graphqlType,
    description?: string,
    deprecationReason?: string,
  }

  type config = {
    name: string,
    astNode?: AstNode.t,
    description?: string,
    fields: unit => fields,
  }
  @module("graphql") @new external make: config => t = "GraphQLInputObjectType"
}

module GraphQLListType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  @module("graphql") @new external make: graphqlType => t = "GraphQLList"
}

module GraphQLUnionType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type resolveUnionTypeFn

  external makeResolveUnionTypeFn: ('source => 'return) => resolveUnionTypeFn = "%identity"

  type config = {
    name: string,
    astNode?: AstNode.t,
    types: unit => array<GraphQLObjectType.t>,
    resolveType: resolveUnionTypeFn,
    description?: string,
  }
  @module("graphql") @new external make: config => t = "GraphQLUnionType"
}

type enumValues

external makeEnumValues: {..} => enumValues = "%identity"

module GraphQLEnumType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type enumValueConfig = {
    value?: string,
    deprecationReason?: string,
    description?: string,
  }

  type config = {
    name: string,
    astNode?: AstNode.t,
    values: enumValues,
    description?: string,
  }
  @module("graphql") @new external make: config => t = "GraphQLEnumType"
}

module GraphQLSchemaType = {
  type t<'appContext>

  @module("graphql") @new
  external make: {..} => t<_> = "GraphQLSchema"

  @module("graphql") external print: t<_> => string = "printSchema"
}
