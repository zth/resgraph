type graphqlType

module Scalars = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  @module("graphql") @val external string: t = "GraphQLString"
}

type arg = {@as("type") typ: graphqlType}

type resolveFn

external makeResolveFn: (('srcValue, 'args, 'ctx) => 'return) => resolveFn = "%identity"

type fields

external makeFields: {..} => fields = "%identity"

type args

external makeArgs: {..} => args = "%identity"

module GraphQLObjectType = {
  type t

  external toGraphQLType: t => graphqlType = "%identity"

  type typeField = {
    @as("type") typ: graphqlType,
    args?: args,
    resolve: resolveFn,
  }
  type config = {
    name: string,
    fields: unit => fields,
  }
  @module("graphql") @new external make: config => t = "GraphQLObjectType"
}

module GraphQLSchema = {
  type t
  @module("graphql") @new
  external make: {..} => t = "GraphQLSchema"
}
