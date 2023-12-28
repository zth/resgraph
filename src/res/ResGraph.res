type id

external id: string => id = "%identity"
external idToString: id => string = "%identity"

type schema<'appContext> = ResGraph__GraphQLJs.GraphQLSchemaType.t<'appContext>

@module("graphql") external printSchema: schema<_> => string = "printSchema"

module GraphQLLiteralValue = ResGraph__GraphQLJs.GraphQLLiteralValue

module JSON = ResGraph__GraphQLJs.GraphQLLiteralValue

module Connections = ResGraph__Connections

module Utils = ResGraph__Utils

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
  variableValues: Dict.t<Js.Json.t>,
}
