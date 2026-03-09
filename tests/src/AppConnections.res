open ResGraph.Connections

@gql.field
let userConnection = (
  _: Query.query,
  ~first: option<int>=?,
  ~after: option<string>=?,
) => {
  let users: array<User.user> = [
    {id: "c1", name: "Alice", age: 28, lastAge: Some(27)},
    {id: "c2", name: "Bob", age: 32, lastAge: None},
  ]

  connectionFromArray(
    users,
    ~args={after, before: None, first, last: None},
  )
}

@gql.field
let nestedConnection = (_: Query.query) => {
  let edge: edge<string> = {node: Some("Hello"), cursor: "c"}
  let pageInfo: pageInfo = {
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
