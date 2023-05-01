---
sidebar_position: 8
---

# Mutations

If your GraphQL API has mutations, you'll need to define a `mutation` type:

```rescript
@gql.type
type mutation
```

Notice that `mutation` is also an _abstract type_, just like [`query`](query).

## Adding fields to `Mutation`

Fields are added to `Mutation` just like you'd [add fields to any other type](object-types#adding-fields-to-types-via-functions):

```rescript
@gql.field
let setNewName = (_: mutation, ~newName, ~userId, ~ctx: ResGraphContext.context) => {
  // Returns promise<option<user>>
  ctx.mutations.setNewUserName(~userId=userId->ResGraph.idToString, ~name=newName)
}
```

```graphql
type Mutation {
  setNewName(newName: String!, userId: ID!): User
}
```

Notice `_: mutation`. ResGraph needs to know that it's `Mutation` you want to attach this field to, but since there's nothing you can actually do with your abstract `mutation` type, it's good practice to name it `_`.

A good next read is [how to define object types](object-types).
