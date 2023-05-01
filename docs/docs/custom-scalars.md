---
sidebar_position: 6
---

# Custom Scalars

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

- Must be a type alias.
- The type alias must be to a type that's a [valid GraphQL type](#valid-graphql-types).

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

The ease of leveraging opaque types is one of the main strengths of ReScript. ResGraph makes it easy to leverage them when building your GraphQL server as well.

To define a custom scalar backed by an abstract type, do this:

```rescript
module Timestamp: {
  /** A timestamp. */
  @gql.scalar
  type t

  let make: unit => t
} = {
  type t = float
  let make = () => Date.now()
}

@gql.field
let currentTime = (_: query) => {
  Timestamp.make()
}
```

```graphql
"""
A timestamp.
"""
scalar Timestamp

type Query {
  currentTime: Timestamp!
}
```

Notice a few things:

- The opaque type must be named exactly `t`, and annotated with `@gql.scalar`.
- The name of the custom scalar will be derived from the _module name_ when the type is `t`.

Notice also that while the type `t` is opaque in your server, you're not forced to show how to serialize and parse your custom scalar if its underlying type is a valid GraphQL type. This is because ResGraph will look at your implementation.

However, there are cases when you either must define a way to parse and serialize a custom scalar, because its underlying represenation isn't a valid GraphQL type. Or when you want to control how the custom scalar is parsed and serialized for other reasons.

In those cases, you can define a custom scalar with a custom parser and serializer:

## Custom scalars that need custom parsing and serializing

You define a custom scalar that needs a custom parser and serializer like this:

```rescript
module Datetime: {
  /** A date. */
  @gql.scalar
  type t

  let parseValue: ResGraph.GraphQLLiteralValue.t => option<t>
  let serialize: t => ResGraph.GraphQLLiteralValue.t
} = {
  type t = Date.t

  open ResGraph.GraphQLLiteralValue

  let parseValue = v =>
    switch v {
    | String(str) => Some(Date.fromString(str))
    | Number(timestamp) => Some(Date.fromTime(timestamp))
    | _ => None
    }

  let serialize = d => d->Date.toJSON->Option.getExn->String
}
```

```graphql
"""
A date.
"""
scalar Datetime
```

Let's distill what's going on here:

- `Datetime.t` is opaque, and the underlying type is `Date.t`, which isn't a [valid GraphQL type](valid-graphql-types).
- We define `parseValue: ResGraph.GraphQLLiteralValue.t => option<t>` and `serialize: t => ResGraph.GraphQLLiteralValue.t`. These need to be defined _exactly_ like this, as in be called those names, and use `GraphQLLiteralValue.t` + the local `t` type.
- `parseValue` is responsible for parsing the value GraphQL gives you at runtime, into your local `t`.
- `serialize` is responsible for turning your `t` into a literal value that can be transferred to the client.

With this, your custom scalar can now be serialized and parsed even if it isn't backed by a valid GraphQL type.
