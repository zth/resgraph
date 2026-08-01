/** Caching metadata consumed by a """schema transform""". */
@gql.directive({locations: ["OBJECT", "FIELD_DEFINITION"], repeatable: false})
type cacheControl = {
  /** Maximum cache lifetime in seconds. */
  @gql.default(60)
  maxAge: int,
  scope: option<string>,
  @deprecated("Use \"scope\" instead.")
  legacyScope: option<string>,
}

@gql.directive({locations: ["OBJECT"]})
type identifies = {
  @gql.default(123)
  value: ResGraph.id,
}

/** Repeatable labels for schema elements. */
@gql.directive({
  locations: [
    "SCALAR",
    "OBJECT",
    "FIELD_DEFINITION",
    "ENUM",
    "ENUM_VALUE",
    "INPUT_OBJECT",
    "INPUT_FIELD_DEFINITION",
  ],
  repeatable: true,
})
type tag = {name: string}

@gql.annotate({name: "tag", args: {name: "input"}})
@gql.inputObject
type directiveInput = {
  @gql.annotate({name: "tag", args: {name: "input-field"}})
  value: string,
}

@gql.annotate({name: "tag", args: {name: "enum"}})
@gql.enum
type directiveStatus =
  | @gql.annotate({name: "tag", args: {name: "enum-value"}}) Active

@gql.annotate({name: "identifies", args: {value: 456}})
@gql.annotate({name: "tag", args: {name: "first"}})
@gql.annotate({name: "cacheControl", args: {maxAge: 30}})
@gql.annotate({name: "tag", args: {name: "second"}})
@gql.type
type directiveExample = {
  @gql.annotate({name: "cacheControl", args: {maxAge: 10, scope: "private"}})
  @gql.annotate({name: "tag", args: {name: "field"}})
  @gql.field
  value: string,
  @gql.field
  status: directiveStatus,
}

@gql.field
let directiveExample = (_: Query.query, ~input: directiveInput): directiveExample => {
  value: input.value,
  status: Active,
}
