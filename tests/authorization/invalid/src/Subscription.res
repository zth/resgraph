@gql.type
type subscription

let makeAsyncIterator: (
  unit => promise<AsyncIterator.value<SubscriptionEvent.subscriptionEvent>>
) => AsyncIterator.t<SubscriptionEvent.subscriptionEvent> = %raw(`function makeAsyncIterator(next) {
    return {next, [Symbol.asyncIterator]() { return this }}
  }`)

@gql.authorize(Security.canSubscribe) @gql.field
let events = (_: subscription): AsyncIterator.t<SubscriptionEvent.subscriptionEvent> =>
  makeAsyncIterator(async () => {
    AsyncIterator.done: true,
    value: Some({SubscriptionEvent.value: "event"}),
  })
