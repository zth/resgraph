type id

external id: string => id = "%identity"
external idToString: id => string = "%identity"

@editor.completeFrom(Execute)
type schema<'appContext> = ResGraph__GraphQLJs.GraphQLSchemaType.t<'appContext>

@module("graphql") external printSchema: schema<_> => string = "printSchema"

module GraphQLLiteralValue = ResGraph__GraphQLJs.GraphQLLiteralValue

module JSON = ResGraph__GraphQLJs.GraphQLLiteralValue

module Connections = ResGraph__Connections

module Utils = ResGraph__Utils

module Execute: {
  type document

  type executionResult<'data, 'error, 'extensions> = {
    data?: 'data,
    errors?: array<'error>,
    extensions?: 'extensions,
  }

  type variables = Dict.t<JSON.t>
  type queryDocumentCache

  let parseQuery: string => document
  let makeQueryDocumentCache: unit => queryDocumentCache
  let setCachedQuery: (~cache: queryDocumentCache, ~query: string, ~document: document) => unit
  let getCachedQuery: (~cache: queryDocumentCache, ~query: string) => option<document>
  let parseQueryCached: (~cache: queryDocumentCache, ~query: string) => document

  let executeParsed: (
    schema<'appContext>,
    ~document: document,
    ~contextValue: 'appContext,
    ~variableValues: variables=?,
    ~operationName: string=?,
    ~rootValue: 'rootValue=?,
  ) => promise<executionResult<'data, 'error, 'extensions>>

  let execute: (
    schema<'appContext>,
    ~query: string,
    ~contextValue: 'appContext,
    ~cache: queryDocumentCache=?,
    ~variableValues: variables=?,
    ~operationName: string=?,
    ~rootValue: 'rootValue=?,
  ) => promise<executionResult<'data, 'error, 'extensions>>
} = {
  type document

  type executionResult<'data, 'error, 'extensions> = {
    data?: 'data,
    errors?: array<'error>,
    extensions?: 'extensions,
  }

  type variables = Dict.t<JSON.t>
  type queryDocumentCache = Dict.t<document>

  type executeArgs<'appContext, 'rootValue> = {
    schema: schema<'appContext>,
    document: document,
    contextValue: 'appContext,
    variableValues?: variables,
    operationName?: string,
    rootValue?: 'rootValue,
  }

  @module("graphql") external parseQuery: string => document = "parse"

  @module("graphql")
  external executeInternal: executeArgs<'appContext, 'rootValue> => promise<
    executionResult<'data, 'error, 'extensions>,
  > = "execute"

  @module("node:crypto") external createHash: 'a = "createHash"

  let hashQuery = query => createHash("sha256")["update"](query)["digest"]("hex")

  let cacheKeyForQuery = query => hashQuery(query)

  let makeQueryDocumentCache = () => Dict.make()

  let setCachedQuery = (~cache, ~query, ~document) => {
    cache->Dict.set(query->cacheKeyForQuery, document)
  }

  let getCachedQuery = (~cache, ~query) => cache->Dict.get(query->cacheKeyForQuery)

  let parseQueryCached = (~cache, ~query) =>
    switch cache->Dict.get(query->cacheKeyForQuery) {
    | Some(document) => document
    | None =>
      let document = parseQuery(query)
      cache->Dict.set(query->cacheKeyForQuery, document)
      document
    }

  let executeParsed = (
    schema,
    ~document,
    ~contextValue,
    ~variableValues=?,
    ~operationName=?,
    ~rootValue=?,
  ) =>
    executeInternal({
      schema,
      document,
      contextValue,
      ?variableValues,
      ?operationName,
      ?rootValue,
    })

  let execute = (
    schema,
    ~query,
    ~contextValue,
    ~cache=?,
    ~variableValues=?,
    ~operationName=?,
    ~rootValue=?,
  ) => {
    let document = switch cache {
    | None => parseQuery(query)
    | Some(cache) => parseQueryCached(~cache, ~query)
    }

    executeParsed(schema, ~document, ~contextValue, ~variableValues?, ~operationName?, ~rootValue?)
  }
}

module ResolveInfo = {
  type token

  type rec source = {
    body: string,
    name: string,
    locationOffset: location,
  }
  and location = {
    start: int,
    end: int,
    startToken: token,
    endToken: token,
    source: source,
  }

  type nameNode = {
    loc?: location,
    value: string,
  }

  // TODO: Extend bindings
  type valueNode

  type argumentNode = {
    loc?: location,
    name: nameNode,
    valueNode: valueNode,
  }

  type directiveNode = {
    loc?: location,
    name: nameNode,
    arguments?: array<argumentNode>,
  }

  // TODO: Bind
  type selectionNode

  type selectionSetNode = {
    loc?: location,
    selections: array<selectionNode>,
  }

  type fieldNode = {
    loc?: location,
    alias?: nameNode,
    name: nameNode,
    arguments?: array<argumentNode>,
    // nullabilityAssertion?: NullabilityAssertionNode.t
    directives?: array<directiveNode>,
  }

  type outputType
  type compositeType
  type schema
  type fragmentDefinition
  type operationDefinition
}

type resolveInfo = {
  fieldName: string,
  fieldNodes: array<ResolveInfo.fieldNode>,
  returnType: ResolveInfo.outputType,
  parentType: ResolveInfo.compositeType,
  schema: ResolveInfo.schema,
  fragments: Dict.t<ResolveInfo.fragmentDefinition>,
  operation: ResolveInfo.operationDefinition,
  variableValues: Dict.t<JSON.t>,
}
