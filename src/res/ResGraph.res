type id

external id: string => id = "%identity"
external idToString: id => string = "%identity"

type schema<'appContext> = ResGraph__GraphQLJs.GraphQLSchemaType.t<'appContext>

@module("graphql") external printSchema: schema<_> => string = "printSchema"

module GraphQLLiteralValue = ResGraph__GraphQLJs.GraphQLLiteralValue

module JSON = ResGraph__GraphQLJs.GraphQLLiteralValue

module Connections = ResGraph__Connections

module Utils = ResGraph__Utils

// TODO: Proper bindings for this
type info
