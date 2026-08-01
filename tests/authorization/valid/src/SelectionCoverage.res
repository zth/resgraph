@gql.authorize(Security.canReadSelectionNode)
@gql.type
type selectionNode = {
  @gql.field
  value: string,
}

@gql.type
type selectionEdge = {
  @gql.field
  cursor: string,
  @gql.field
  node: selectionNode,
}

@gql.type
type selectionConnection = {
  @gql.field
  edges: array<selectionEdge>,
}
