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
    | Object(dict<t>)
    | Array(array<t>)
}

type directiveArguments = Dict.t<GraphQLLiteralValue.t>
type directiveMap = Dict.t<array<directiveArguments>>
type appliedDirective = {name: string, args: directiveArguments}
type resgraphDirectiveExtensions = {appliedDirectives: array<appliedDirective>}
type directiveExtensions = {
  directives?: directiveMap,
  resgraph?: resgraphDirectiveExtensions,
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

type arg = {
  @as("type") typ: graphqlType,
  defaultValue?: GraphQLLiteralValue.t,
  description?: string,
  deprecationReason?: string,
  extensions?: directiveExtensions,
}

type resolveFn

external makeResolveFn: (('srcValue, 'args, 'ctx, 'info) => 'return) => resolveFn = "%identity"

type fields

external makeFields: {..} => fields = "%identity"

type args

external makeArgs: {..} => args = "%identity"
external makeArgsDict: Dict.t<arg> => args = "%identity"

type typeField = {
  @as("type") typ: graphqlType,
  args?: args,
  resolve?: resolveFn,
  description?: string,
  deprecationReason?: string,
  subscribe?: resolveFn,
  extensions?: directiveExtensions,
}

module GraphQLDirective = {
  type t

  type config = {
    name: string,
    description?: string,
    locations: array<string>,
    args?: args,
    isRepeatable?: bool,
  }

  @module("graphql") @new external make: config => t = "GraphQLDirective"
  @module("graphql") @val external specifiedDirectives: array<t> = "specifiedDirectives"
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
    extensions?: directiveExtensions,
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
    specifiedByURL?: string,
    extensions?: directiveExtensions,
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
    extensions?: directiveExtensions,
  }
  @module("graphql") @new external make: config => t = "GraphQLObjectType"
}

module GraphQLInputObjectType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type inputObjectField = {
    @as("type") typ: graphqlType,
    defaultValue?: GraphQLLiteralValue.t,
    description?: string,
    deprecationReason?: string,
    extensions?: directiveExtensions,
  }

  type config = {
    name: string,
    astNode?: AstNode.t,
    description?: string,
    fields: unit => fields,
    isOneOf?: bool,
    extensions?: directiveExtensions,
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
    extensions?: directiveExtensions,
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
    extensions?: directiveExtensions,
  }

  type config = {
    name: string,
    astNode?: AstNode.t,
    values: enumValues,
    description?: string,
    extensions?: directiveExtensions,
  }
  @module("graphql") @new external make: config => t = "GraphQLEnumType"
}

module GraphQLSchemaType = {
  type t<'appContext>

  type config = {
    description?: string,
    query: GraphQLObjectType.t,
    mutation?: GraphQLObjectType.t,
    subscription?: GraphQLObjectType.t,
    types?: array<graphqlType>,
    directives?: array<GraphQLDirective.t>,
    extensions?: directiveExtensions,
  }

  // Keep the legacy open-object constructor so previously generated schemas
  // still compile during an upgrade. New codegen uses the typed constructor.
  @module("graphql") @new external make: {..} => t<_> = "GraphQLSchema"
  @module("graphql") @new external makeConfig: config => t<_> = "GraphQLSchema"

  @module("graphql") external print: t<_> => string = "printSchema"
}
