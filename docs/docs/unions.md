---
sidebar_position: 4
---

# Unions

You define a union by defining a variant with payloads of types tagged with `@gql.type`, and annotate that variant with `@gql.union`:

```rescript
@gql.type
type user = {
  @gql.field name: string,
  @gql.field age: int
}

@gql.type
type group = {
  @gql.field displayName: string,
}

@gql.union
type entity = User(user) | Group(group)
```

```graphql
type User {
  name: String!
  age: Int!
}

type Group {
  displayName: String!
}

union Entity = User | Group
```

Each variant case can be called whatever you want it to (although it's good practice to follow the name of the GraphQL type it holds), but rememeber that the payload of each union variant case must be one of:

- Exactly 1 type that has a `@gql.type` annotation
- An inline record

But don't worry, ResGraph will complain if you try anything else.

You can add comments on the union type definition itself, as well as each variant case, and they'll end up in the schema.

### Inline records for defining union members

You can create ad hoc "synthetic" object types for your unions by using an _inline record_ as the payload for a variant case in union. Let's look at an example:

```rescript
@gql.union
type setPasswordPayload = Ok({affectedUser: user}) | Failed({reason: string})
```

This defines a variant called `setPasswordPayload` with two cases - `Ok` and `Failed`. Each of those cases also has additional fields through an inline record. The above will generate the following GraphQL:

```graphql
union SetPasswordPayload
{
  SetPasswordPayloadOk
  SetPasswordPayloadFailed
}

type SetPasswordPayloadFailed {
  reason: String!
}

type SetPasswordPayloadOk {
  affectedUser: User!
}
```

Notice a few things:

- Each inline record has been synthesized into an actual GraphQL type holding _all_ fields that were defined in that inline record.
- The synthetiszed GraphQL types are called `<unionName><caseName>`. `Ok` therefore becomes `SetPasswordPayloadOk`.

This is intended to be a quick way to define one-off GraphQL types only intended to be used in a specific enum, like how you'd typically design a result from a mutation.

## Using unions in the schema

Unions can be used as the type for fields on GraphQL objects or interfaces. A simple example:

```rescript
@gql.type
type user = {
  @gql.field name: string,
  @gql.field age: int
}

@gql.type
type group = {
  @gql.field displayName: string,
}

@gql.union
type entity = User(user) | Group(group)

@gql.field
let entity = async (_: query, ~entityId, ~ctx: ResGraphContext.context): option<entity> => {
  switch decodeEntityId(entityId) {
  | Some(#User, id) =>
    switch await ctx.dataLoaders.userById(~userId=id) {
    | None => None
    | Some(user) => Some(User(user))
    }
  | Some(#Group, id) =>
    switch await ctx.dataLoaders.groupById(~groupId=id) {
    | None => None
    | Some(group) => Some(Group(group))
    }
  | _ => None
  }
}

```

```graphql
type User {
  name: String!
  age: Int!
}

type Group {
  displayName: String!
}

union Entity = User | Group

type Query {
  entity(entityId: String!): Entity
}
```

Now that we've covered unions, we can move on to [interfaces](interfaces).
