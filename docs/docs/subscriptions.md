---
sidebar_position: 9
---

# Subscriptions

You can add a subscription to your schema by defining a `subscription` type and then attaching resolvers to it. All subscription resolvers _must return an [`AsyncIterator.t`](https://rescript-lang.org/docs/manual/latest/api/core/asynciterator)_ with a valid GraphQL type.

Let's look at an example:

```rescript
// Define the subscription type
@gql.type
type subscription

let wait = ms => {
  Promise.make((resolve, _) => {
    let _ = setTimeout(() => resolve(), ms)
  })
}

@gql.field
let countdown = (_: subscription, ~from: int) => {
  let countdown = ref(from)
  let iterator = AsyncIterator.make(async () => {
    await wait(500)
    let current = countdown.contents
    countdown := current - 1

    if current > 0 {
      AsyncIterator.value(current)
    } else {
      AsyncIterator.done(~finalValue=current)
    }
  })

  iterator
}

```

This would produce the following schema:

```graphql
type Subscription {
  countdown(from: Int!): Int!
}
```

## Async lists in queries and mutations

Outside subscriptions, a resolver returning
`ResGraph.AsyncIterable.t<'value>` (or the standard `AsyncIterable.t` where the
ReScript version provides it) maps to a normal non-null GraphQL list. ResGraph
passes the async iterable through so an incremental-delivery executor can
consume it:

```rescript
@gql.field
let events = (_: Query.query): ResGraph.AsyncIterable.t<event> => loadEvents()
```

```graphql
type Query {
  events: [Event!]!
}
```

This shape requires a server/executor that implements incremental `@stream`
delivery. The normal GraphQL 16 executor expects an in-memory iterable and
will reject the async iterable. `@stream` is an executable operation directive,
so enabling its execution and transport behavior remains the responsibility of
the GraphQL server rather than the ResGraph schema compiler.
