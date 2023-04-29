The heart of your GraphQL server is the `Query` type. The first thing you'll do setting up your GraphQL server is to define your query type:

```rescript
@gql.type
type query
```

Notice that `query` is an _abstract type_. In some flavors of GraphQL, `query` is allowed to have a root value, just like regular types also can. However, in ResGraph, we've opted to reduce complexity by allowing neither `Query` nor `Mutation` to have root values.

Instead you're encouraged to put whatever you'd put in the root value for `Query` or `Mutation` into your `ResGraphContext.context`.

## Adding fields to `Query`

Fields are added to `Query` just like you'd [add fields to any other type](object-types#adding-fields-to-types-via-functions):

```rescript
@gql.field
let currentTime = (_: query) => {
  Date.now()
}
```

```graphql
type Query {
  currentTime: Float!
}
```

Notice `_: query`. ResGraph needs to know that it's `Query` you want to attach this field to, but since there's nothing you can actually do with your abstract `query` type, it's good practice to name it `_`.

A good next read is [how to define object types](object-types).
