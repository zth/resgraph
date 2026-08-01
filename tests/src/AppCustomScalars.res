@gql.inputObject
type scalarMetadataInput = {label: string}

@gql.enum
type scalarTier = Premium

@gql.directive({locations: ["SCALAR"]})
type scalarMetadata = {config: scalarMetadataInput, tier: scalarTier}

/** Custom scalar with specifiedByUrl coverage. */
@specifiedBy("https://example.com/specifiedBy/uuid")
@gql.annotate({
  name: "scalarMetadata",
  args: {config: {label: "deferred"}, tier: Premium},
})
@gql.annotate({name: "tag", args: {name: "scalar"}})
@gql.scalar
type uuid = string
// ^hov

@gql.type
type scalarHolder = {
  @gql.field id: uuid,
}

@gql.field
let getScalarHolder = (_: Query.query): scalarHolder => {id: "abc"}
// ^hov

module LiteralText = {
  /** Text scalar with explicit literal coercion. */
  @gql.scalar
  type t = string

  let parseValue = value =>
    switch value {
    | ResGraph.GraphQLLiteralValue.String(value) => Some(value)
    | _ => None
    }

  let parseLiteral = node => node->ResGraph.GraphQLValueNode.toLiteralValue->parseValue

  let serialize = value => ResGraph.GraphQLLiteralValue.String(value)
}

@gql.query
let literalText = (~value: LiteralText.t): LiteralText.t => value

module DefaultText = {
  @gql.scalar
  type t = string

  let parseValue = value =>
    switch value {
    | ResGraph.GraphQLLiteralValue.String(value) => Some("parsed:" ++ value)
    | _ => None
    }

  let parseLiteral = node => node->ResGraph.GraphQLValueNode.toLiteralValue->parseValue

  let serialize = value => ResGraph.GraphQLLiteralValue.String(value)
}

@gql.query
let defaultText = (~value: DefaultText.t="source"): DefaultText.t => value
