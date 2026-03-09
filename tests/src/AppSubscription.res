@gql.type
type subscription

let makeAsyncIterator: (unit => promise<AsyncIterator.value<string>>) => AsyncIterator.t<
  string,
> = %raw(`function makeAsyncIterator(next) {
  return {
    next,
    [Symbol.asyncIterator]() {
      return this
    },
  }
}`)

@gql.field
let latestMessage = (
  _: subscription,
  ~ctx: ResGraphContext.context,
): AsyncIterator.t<string> => {
  ignore(ctx)
  makeAsyncIterator(async () => {
    {
      AsyncIterator.done: true,
      value: Some("ping"),
    }
  })
}
