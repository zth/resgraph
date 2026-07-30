@gql.type
type subscription

let makeAsyncIterator: (unit => promise<AsyncIterator.value<string>>) => AsyncIterator.t<
  string,
> = %raw(`function makeAsyncIterator(next) {
    return {next, [Symbol.asyncIterator]() { return this }}
  }`)

@gql.public({reason: "Existing public stream"}) @gql.field
let events = (_: subscription): AsyncIterator.t<string> =>
  makeAsyncIterator(async () => {AsyncIterator.done: true, value: Some("event")})
