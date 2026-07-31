type id

external id: string => id = "%identity"
external idToString: id => string = "%identity"

@editor.completeFrom(Execute)
type schema<'appContext> = ResGraph__GraphQLJs.GraphQLSchemaType.t<'appContext>

@module("graphql") external printSchema: schema<_> => string = "printSchema"

module GraphQLLiteralValue = ResGraph__GraphQLJs.GraphQLLiteralValue
module GraphQLJSON = ResGraph__GraphQLJs.GraphQLLiteralValue

module Connections = ResGraph__Connections

module Utils = ResGraph__Utils

module Authorization = {
  type outcome<'value, 'reason> =
    | Allowed('value)
    | Forbidden('reason)

  type error = {message: string, code: string}
  type errorExtensions = {code: string}
  type errorOptions = {extensions: errorExtensions}
  type graphqlError = exn

  @module("graphql") @new
  external makeGraphQLError: (string, ~options: errorOptions) => graphqlError = "GraphQLError"

  let makeError = (~message, ~code): error => {message, code}

  let raiseError = (error: error): 'value =>
    throw(makeGraphQLError(error.message, ~options={extensions: {code: error.code}}))

  let raiseForbidden = (_reason: 'reason): 'value =>
    raiseError(makeError(~message="Forbidden", ~code="FORBIDDEN"))
}

module Execute: {
  type document

  type executionResult<'data, 'error, 'extensions> = {
    data?: 'data,
    errors?: array<'error>,
    extensions?: 'extensions,
  }

  type jsonExecutionResult = executionResult<JSON.t, JSON.t, JSON.t>

  type variables = Dict.t<JSON.t>
  type queryDocumentCache

  let parseQuery: string => document
  let makeQueryDocumentCache: unit => queryDocumentCache
  let makeQueryDocumentCacheWithMaxSize: (~maxSize: int) => queryDocumentCache
  let setCachedQuery: (~cache: queryDocumentCache, ~query: string, ~document: document) => unit
  let getCachedQuery: (~cache: queryDocumentCache, ~query: string) => option<document>
  let parseQueryCached: (~cache: queryDocumentCache, ~query: string) => document
  let variablesFromJson: JSON.t => option<variables>
  let variablesToJson: variables => JSON.t
  let executionResultToJson: jsonExecutionResult => JSON.t

  let executeParsed: (
    schema<'appContext>,
    ~document: document,
    ~contextValue: 'appContext,
    ~variableValues: variables=?,
    ~operationName: string=?,
    ~rootValue: 'rootValue=?,
  ) => promise<executionResult<'data, 'error, 'extensions>>

  let executeParsedToJson: (
    schema<'appContext>,
    ~document: document,
    ~contextValue: 'appContext,
    ~variablesJson: JSON.t=?,
    ~operationName: string=?,
    ~rootValue: 'rootValue=?,
  ) => promise<JSON.t>

  let execute: (
    schema<'appContext>,
    ~query: string,
    ~contextValue: 'appContext,
    ~cache: queryDocumentCache=?,
    ~variableValues: variables=?,
    ~operationName: string=?,
    ~rootValue: 'rootValue=?,
  ) => promise<executionResult<'data, 'error, 'extensions>>

  let executeToJson: (
    schema<'appContext>,
    ~query: string,
    ~contextValue: 'appContext,
    ~cache: queryDocumentCache=?,
    ~variablesJson: JSON.t=?,
    ~operationName: string=?,
    ~rootValue: 'rootValue=?,
  ) => promise<JSON.t>
} = {
  type document

  type executionResult<'data, 'error, 'extensions> = {
    data?: 'data,
    errors?: array<'error>,
    extensions?: 'extensions,
  }

  type jsonExecutionResult = executionResult<JSON.t, JSON.t, JSON.t>

  type variables = Dict.t<JSON.t>
  type queryDocumentCache

  type executeArgs<'appContext, 'rootValue> = {
    schema: schema<'appContext>,
    document: document,
    contextValue: 'appContext,
    variableValues?: variables,
    operationName?: string,
    rootValue?: 'rootValue,
  }

  @module("graphql") external parseQuery: string => document = "parse"

  @module("./ResGraph__ExecuteRuntime.mjs")
  external executeInternal: executeArgs<'appContext, 'rootValue> => promise<
    executionResult<'data, 'error, 'extensions>,
  > = "executeValidated"

  external variablesOfJsonObject: dict<JSON.t> => variables = "%identity"
  external variablesToJsonObject: variables => dict<JSON.t> = "%identity"
  external executionResultToJson: jsonExecutionResult => JSON.t = "%identity"

  @module("./ResGraph__ExecuteRuntime.mjs")
  external makeQueryDocumentCache: unit => queryDocumentCache = "createQueryDocumentCache"
  @module("./ResGraph__ExecuteRuntime.mjs")
  external makeQueryDocumentCacheWithMaxSize: (~maxSize: int) => queryDocumentCache =
    "createQueryDocumentCache"
  @module("./ResGraph__ExecuteRuntime.mjs")
  external setCachedQuery: (~cache: queryDocumentCache, ~query: string, ~document: document) => unit =
    "setCachedQuery"
  @module("./ResGraph__ExecuteRuntime.mjs")
  external getCachedQuery: (~cache: queryDocumentCache, ~query: string) => option<document> =
    "getCachedQuery"

  let parseQueryCached = (~cache, ~query) =>
    switch getCachedQuery(~cache, ~query) {
    | Some(document) => document
    | None =>
      let document = parseQuery(query)
      setCachedQuery(~cache, ~query, ~document)
      document
    }

  let invalidVariablesJson = () =>
    JSON.Object(dict{
      "errors": JSON.Array([
        JSON.Object(dict{
          "message": JSON.String("GraphQL variables must be a JSON object or null."),
        }),
      ]),
    })

  let variablesFromJson = json =>
    switch json {
    | JSON.Object(jsonObject) => Some(jsonObject->variablesOfJsonObject)
    | JSON.Null => None
    | _ => None
    }

  let variablesToJson = variables => JSON.Object(variables->variablesToJsonObject)

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

  let executeParsedToJson = (
    schema,
    ~document,
    ~contextValue,
    ~variablesJson=?,
    ~operationName=?,
    ~rootValue=?,
  ) => {
    switch variablesJson {
    | Some(JSON.Null) | None =>
      executeParsed(
        schema,
        ~document,
        ~contextValue,
        ~operationName?,
        ~rootValue?,
      )->Promise.thenResolve(executionResultToJson)
    | Some(json) =>
      switch json->variablesFromJson {
      | Some(variableValues) =>
        executeParsed(
          schema,
          ~document,
          ~contextValue,
          ~variableValues,
          ~operationName?,
          ~rootValue?,
        )->Promise.thenResolve(executionResultToJson)
      | None => Promise.resolve(invalidVariablesJson())
      }
    }
  }

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

  let executeToJson = (
    schema,
    ~query,
    ~contextValue,
    ~cache=?,
    ~variablesJson=?,
    ~operationName=?,
    ~rootValue=?,
  ) => {
    switch variablesJson {
    | Some(JSON.Null) | None =>
      execute(
        schema,
        ~query,
        ~contextValue,
        ~cache?,
        ~operationName?,
        ~rootValue?,
      )->Promise.thenResolve(executionResultToJson)
    | Some(json) =>
      switch json->variablesFromJson {
      | Some(variableValues) =>
        execute(
          schema,
          ~query,
          ~contextValue,
          ~cache?,
          ~variableValues,
          ~operationName?,
          ~rootValue?,
        )->Promise.thenResolve(executionResultToJson)
      | None => Promise.resolve(invalidVariablesJson())
      }
    }
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
  variableValues: Dict.t<GraphQLJSON.t>,
}
