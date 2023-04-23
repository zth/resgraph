A custom scalar is a scalar type in your schema where the underlying value is (somewhat) opaque to the client.

Custom scalars are defined using the `@gql.scalar` attribute in ResGraph. Here follows a description of how you can leverage simple and more advanced type setups for custom scalars.

## Simple custom scalars

The simplest way to define a custom scalar is to use a type alias:

```rescript
/** A timestamp. */
@gql.scalar
type timestamp = float
```

```graphql
"""
A timestamp.
"""
scalar Timestamp
```

A simple custom scalar has the following restrictions:

- Must be a type alias
- The type alias must be to a type that's a [valid GraphQL type](#valid-graphql-types)

### Caveats

Because this is a type alias, you'll need to use explicit type annotations to tell ResGraph that you're looking to use your new scalar type rather than the underlying primitive.

```rescript
@gql.scalar
type timestamp = float

@gql.field
let currentTime = (_: query) => {
  Date.now()
}
```

```graphql
scalar Timestamp

type Query {
  currentTime: Float!
}
```

Notice that there's no way for ResGraph to know that you meant to use your `Timestamp` scalar, and not `float`. You solve this either by annotating the function return type with `timestamp`, or by assinging the value you want to return to a value that's annotated with `timestamp`, and then return that value. Both examplified here:

```rescript
@gql.scalar
type timestamp = float

@gql.field
let currentTime = (_: query): timestamp => {
  // Either annotate the return type, like above,
  // or annotate an intermediate value, like below:
  let time: timestamp = Date.now()
  time
}
```

Now ResGraph understands that you're returning a custom scalar:

```graphql
scalar Timestamp

type Query {
  currentTime: Timestamp!
}
```

## Opaque types and custom scalars

## Custom scalars that need custom parsing and serializing
