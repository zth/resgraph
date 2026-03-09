@gql.type
type subscription

@gql.field
let latestMessage = async (_: subscription, ~ctx: ResGraphContext.context) => {
  ignore(ctx)
  Promise.resolve("ping")
}
