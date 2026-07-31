/** A root query without an unused source argument. */
@gql.query
let shorthandGreeting = () => "hello"

@gql.query
let shorthandEcho = (~message: string) => message

@gql.query
let shorthandContext = (~ctx: ResGraphContext.context) => {
  ignore(ctx)
  "context"
}

@gql.mutation
let shorthandIncrement = (~value: int) => value + 1

@gql.subscription
let shorthandLatest = (): AsyncIterator.t<string> =>
  AppSubscription.makeAsyncIterator(async () => {
    {
      AsyncIterator.done: true,
      value: Some("latest"),
    }
  })
