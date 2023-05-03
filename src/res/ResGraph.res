type id

external id: string => id = "%identity"
external idToString: id => string = "%identity"

type schema<'appContext> = ResGraph__GraphQLJs.GraphQLSchemaType.t<'appContext>

@module("graphql") external printSchema: schema<_> => string = "printSchema"

module GraphQLLiteralValue = ResGraph__GraphQLJs.GraphQLLiteralValue

module JSON = ResGraph__GraphQLJs.GraphQLLiteralValue

module Connections = {
  /** Information about pagination in a connection. */
  @gql.type
  type pageInfo = {
    /** When paginating forwards, are there more items? */
    @gql.field
    hasNextPage: bool,
    /** When paginating backwards, are there more items? */
    @gql.field
    hasPreviousPage: bool,
    /** When paginating backwards, the cursor to continue. */
    @gql.field
    startCursor: option<string>,
    /** When paginating forwards, the cursor to continue. */
    @gql.field
    endCursor: option<string>,
  }
}
