type id

external id: string => id = "%identity"

type schema<'appContext> = ResGraph__GraphQLJs.GraphQLSchemaType.t

@module("graphql") external printSchema: schema<_> => string = "printSchema"
