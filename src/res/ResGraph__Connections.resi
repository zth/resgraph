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

/** An edge in a connection. */
type edge<'node> = {
  /** The item at the end of the edge. */
  @gql.field
  node: option<'node>,
  /** A cursor for use in pagination. */
  @gql.field
  cursor: string,
}

/** A connection to a list of items. */
type connection<'edge> = {
  /** Information to aid in pagination. */
  @gql.field
  pageInfo: pageInfo,
  /** A list of edges. */
  @gql.field
  edges: option<array<option<'edge>>>,
}

/** A connection to a list of items. */
type connectionWithExtra<'edge, 'extra> = {
  /** Information to aid in pagination. */
  @gql.field
  pageInfo: pageInfo,
  /** A list of edges. */
  @gql.field
  edges: option<array<option<'edge>>>,
  extra: 'extra,
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

/** TODO: Docs */
let connectionFromArrayWithExtra: (
  array<'a>,
  ~args: connectionArgs,
  ~extra: 'b,
) => connectionWithExtra<edge<'a>, 'b>
