@gql.type
type subscriptionEvent = {
  @gql.authorize.byAncestor({reason: "Subscription events require per-event authorization"})
  @gql.field
  value: string,
}
