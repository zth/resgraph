@gql.type
type userEdge = {
  @gql.field
  cursor: string,
  @gql.field
  node: option<User.user>,
}

@gql.type
type userConnection = {
  @gql.field
  pageInfo: ResGraph.Connections.pageInfo,
  @gql.field
  edges: option<array<option<userEdge>>>,
}

@gql.type
type stringEdge = {
  @gql.field
  cursor: string,
  @gql.field
  node: option<string>,
}

@gql.type
type stringConnection = {
  @gql.field
  pageInfo: ResGraph.Connections.pageInfo,
  @gql.field
  edges: option<array<option<stringEdge>>>,
}

@gql.field
let userConnection = (
  _: Query.query,
  ~first: option<int>=?,
  ~after: option<string>=?,
): userConnection => {
  ignore(first)
  ignore(after)

  let alice: User.user = {id: "c1", name: "Alice", age: 28, lastAge: Some(27)}
  let bob: User.user = {id: "c2", name: "Bob", age: 32, lastAge: None}

  let edges: array<option<userEdge>> = [
    Some({cursor: "c1", node: Some(alice)}),
    Some({cursor: "c2", node: Some(bob)}),
  ]
  let pageInfo: ResGraph.Connections.pageInfo = {
    hasNextPage: false,
    hasPreviousPage: false,
    startCursor: Some("c1"),
    endCursor: Some("c2"),
  }

  {
    pageInfo,
    edges: Some(edges),
  }
}

@gql.field
let nestedConnection = (_: Query.query): stringConnection => {
  let edge: stringEdge = {node: Some("Hello"), cursor: "c"}
  let pageInfo: ResGraph.Connections.pageInfo = {
    hasNextPage: false,
    hasPreviousPage: false,
    startCursor: Some("c"),
    endCursor: Some("c"),
  }
  {
    pageInfo,
    edges: Some([Some(edge)]),
  }
}
