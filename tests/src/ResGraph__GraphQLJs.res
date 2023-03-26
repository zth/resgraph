type graphqlType

@module("graphql") @new external nonNull: graphqlType => graphqlType = "GraphQLNonNull"

module Scalars = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  @module("graphql") @val external string: t = "GraphQLString"
  @module("graphql") @val external int: t = "GraphQLInt"
  @module("graphql") @val external id: t = "GraphQLID"
  @module("graphql") @val external float: t = "GraphQLFloat"
  @module("graphql") @val external boolean: t = "GraphQL"
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
}

module GraphQLObjectType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type config = {
    name: string,
    fields: unit => fields,
  }
  @module("graphql") @new external make: config => t = "GraphQLObjectType"
}

module GraphQLSchemaType = {
  type t
  @module("graphql") @new
  external make: {..} => t = "GraphQLSchema"
}
