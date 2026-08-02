@gql.type
type selectionNode = {
  @gql.authorize.byAncestor({
    reason: "Value has no authorization identity beyond its selection result",
  })
  @gql.field
  value: string,
  @gql.authorize(Security.canReadSelectionNode) @gql.field
  secret: string,
}

@gql.authorize.byAncestor({reason: "Structural edge fields are governed by the selection query"})
@gql.type
type selectionEdge = {
  @gql.field
  cursor: string,
  @gql.field
  node: selectionNode,
}

@gql.authorize.byAncestor({
  reason: "Structural connection fields are governed by the selection query",
})
@gql.type
type selectionConnection = {
  @gql.field
  edges: array<selectionEdge>,
  @gql.public({reason: "Static label contains no protected data"}) @gql.field
  label: string,
}

@gql.authorize.byAncestor({
  reason: "Payload is exposed only after its resolver outcome allows access",
})
@gql.type
type outcomePayload = {
  @gql.field
  value: string,
}
