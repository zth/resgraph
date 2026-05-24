/** Extra coverage for ReScript 12 CMT/attribute changes. */
@gql.type
type res12Record = {
  /** Doc should survive on fields. */
  @gql.field
  withDoc: string,
  /** Deprecated attribute should survive too. */
  @deprecated("old field") @gql.field
  oldField: int,
}

@gql.inputUnion
type res12Input =
  /** Inline record doc is preserved. */
  | Inline({
      /** Field doc on inline record. */
      payload: string,
    })
  | Empty

/** Reserved ReScript field names can be exposed as GraphQL names. */
@gql.type
type reservedWordRecord = {
  @as("constraint") @gql.field
  constraint_: string,
  @as("external") @gql.field
  external_: string,
  @as("include") @gql.field
  include_: string,
  @as("let") @gql.field
  let_: string,
  @as("module") @gql.field
  module_: string,
  @as("open") @gql.field
  open_: string,
  @as("switch") @gql.field
  switch_: string,
  @as("type") @gql.field
  type_: string,
}

@gql.inputObject
type reservedWordInput = {
  @as("constraint")
  constraint_: string,
  @as("type")
  type_: string,
}

@gql.field
let reservedWordRecord = (_: Query.query): reservedWordRecord => {
  constraint_: "constraint",
  external_: "external",
  include_: "include",
  let_: "let",
  module_: "module",
  open_: "open",
  switch_: "switch",
  type_: "type",
}

@gql.field
let reservedWordInputEcho = (_: Query.query, ~input: reservedWordInput) =>
  input.constraint_ ++ ":" ++ input.type_
