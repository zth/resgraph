type graphqlType

@module("graphql") @new external nonNull: graphqlType => graphqlType = "GraphQLNonNull"

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

external makeResolveFn: (('srcValue, 'args, 'ctx) => 'return) => resolveFn = "%identity"

type fields

external makeFields: {..} => fields = "%identity"

type args

external makeArgs: {..} => args = "%identity"

type typeField = {
  @as("type") typ: graphqlType,
  args?: args,
  resolve: resolveFn,
  description?: string,
  deprecationReason?: string,
}

module GraphQLObjectType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type config = {
    name: string,
    description?: string,
    fields: unit => fields,
  }
  @module("graphql") @new external make: config => t = "GraphQLObjectType"
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
    values: enumValues,
    description?: string,
  }
  @module("graphql") @new external make: config => t = "GraphQLEnumType"
}

module GraphQLSchemaType = {
  type t

  @module("graphql") @new
  external make: {..} => t = "GraphQLSchema"

  @module("graphql") external print: t => string = "printSchema"
}
