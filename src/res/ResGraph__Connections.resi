/** Information about pagination in a connection. */
@gql.type
type pageInfo = {
  /** When paginating forwards, are there more items? */
  @gql.public({
    reason: "Structural pagination metadata; the parent connection field controls access",
  })
  @gql.field
  hasNextPage: bool,
  /** When paginating backwards, are there more items? */
  @gql.public({
    reason: "Structural pagination metadata; the parent connection field controls access",
  })
  @gql.field
  hasPreviousPage: bool,
  /** When paginating backwards, the cursor to continue. */
  @gql.public({
    reason: "Structural pagination metadata; the parent connection field controls access",
  })
  @gql.field
  startCursor: option<string>,
  /** When paginating forwards, the cursor to continue. */
  @gql.public({
    reason: "Structural pagination metadata; the parent connection field controls access",
  })
  @gql.field
  endCursor: option<string>,
}

/** An edge in a connection. */
type edge<'node> = {
  /** The item at the end of the edge. */
  @gql.public({
    reason: "Structural connection wrapper; the parent connection field controls access",
  })
  @gql.field
  node: option<'node>,
  /** A cursor for use in pagination. */
  @gql.public({
    reason: "Structural connection wrapper; the parent connection field controls access",
  })
  @gql.field
  cursor: string,
}

/** A connection to a list of items. */
type connection<'edge> = {
  /** Information to aid in pagination. */
  @gql.public({
    reason: "Structural connection wrapper; the parent connection field controls access",
  })
  @gql.field
  pageInfo: pageInfo,
  /** A list of edges. */
  @gql.public({
    reason: "Structural connection wrapper; the parent connection field controls access",
  })
  @gql.field
  edges: option<array<option<'edge>>>,
}

/** TODO: Docs */
type forwardConnectionArgs = {
  after: option<string>,
  first: option<int>,
}

/** TODO: Docs */
type backwardConnectionArgs = {
  before: option<string>,
  last: option<int>,
}

/** TODO: Docs */
type connectionArgs = {
  ...forwardConnectionArgs,
  ...backwardConnectionArgs,
}

/** TODO: Docs */
let connectionFromArray: (array<'node>, ~args: connectionArgs) => connection<edge<'node>>
