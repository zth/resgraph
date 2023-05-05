@gql.type /** Information about pagination in a connection. */
type pageInfo = {
  @gql.field /** When paginating forwards, are there more items? */ hasNextPage: bool,
  @gql.field /** When paginating backwards, are there more items? */ hasPreviousPage: bool,
  @gql.field /** When paginating backwards, the cursor to continue. */ startCursor: option<string>,
  @gql.field /** When paginating forwards, the cursor to continue. */ endCursor: option<string>,
}

/** An edge in a connection. */
type edge<'node> = {
  @gql.field /** The item at the end of the edge. */ node: option<'node>,
  @gql.field /** A cursor for use in pagination. */ cursor: string,
}

/** A connection to a list of items. */
type connection<'edge> = {
  @gql.field /** Information to aid in pagination. */ pageInfo: pageInfo,
  @gql.field /** A list of edges. */ edges: option<array<option<'edge>>>,
}
