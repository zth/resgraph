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

- Exactly 1 type that has a `@gql.type` annotation.
- An inline record.

Don't worry, ResGraph will complain if you try anything else.

You can add comments on the union type definition itself, as well as each variant case, and they'll end up in the schema.

### Inline records for defining union members

You can create ad hoc "synthetic" object types for your unions by using an _inline record_ as the payload for a variant case in union. Let's look at an example:

```rescript
@gql.union
type setPasswordPayload = Ok({affectedUser: user}) | Failed({reason: string})
```

This defines a variant called `setPasswordPayload` with two cases - `Ok` and `Failed`. Each of those cases also has additional fields through an inline record. The above will generate the following GraphQL:

```graphql
union SetPasswordPayload = SetPasswordPayloadOk | SetPasswordPayloadFailed

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

## Advanced: Inferred unions

ResGraph can _infer_ unions from [polymorphic variants](https://rescript-lang.org/docs/manual/latest/polymorphic-variant) in ReScript. This can be very useful when protoyping and working with ResGraph, and can speed you up considerably - get unions without having to declare them.

Let's look at an example:

```rescript
let bestFriend = (user: user) => {
  switch await ctx.db.user.bestFriend(~userId) {
  | Ok(User(user)) => Some(#User(user))
  | Ok(Dog(dog)) => Some(#Dog(dog))
  | Error(_) => None
  }
}
```

This produces this schema:

```graphql
union UserBestFriend = User | Dog

type User {
  bestFriend: UserBestFriend
}
```

It infers that you're returning either a `User` or a `Dog` by looking at the type _inside_ of the polyvariant you return. It then creates a union automatically for that, named after the type + field name it was found on.

This is neat! This means you can quickly produce a union without having to declare them beforehand.

### Inferring full union members

However, to make this really nice, we can combine this with _inferring full objects in union members_ to very easily add ah hoc unions. This is particularly efficient in mutations. Example:

```rescript
let userUpdateName = (
  _: mutation,
  ~userId: ResGraph.id,
  ~newName,
  ~ctx: ResGraphContext.context,
) => {
  switch await ctx.db.updateUserName(
    ~userId=ResGraph.idToString(userId),
    ~name=newName,
  ) {
  | Ok(updatedUser) => Some(#UserNameWasUpdated({"updatedUser": updatedUser}))
  | Error(Unauthorized) =>
    Some(#UserNameUpdateFailed({"message": "Not authorized."}))
  | Error(_) =>
    Some(#UserNameUpdateFailed({"message": "Something went wrong."}))
  }
}

```

This produces this schema:

```graphql
type UserNameWasUpdated {
  updatedUser: User!
}

type UserNameUpdateFailed {
  message: String!
}

union UserUpdateName = UserNameWasUpdated | UserNameUpdateFailed

type Mutation {
  userUpdateName(userId: ID!, newName: String!): UserUpdateName
}
```

Pretty neat. We now have an ad hoc union for our mutation with 0 boilerplate.

#### Going beyond: More inference!

As a little side note, we can extend this even further with more inference. Let's add an inferred enum as well to our result type instead of a message string:

```rescript
let userUpdateName = (
  _: mutation,
  ~userId: ResGraph.id,
  ~newName,
  ~ctx: ResGraphContext.context,
) => {
  switch await ctx.db.updateUserName(
    ~userId=ResGraph.idToString(userId),
    ~name=newName,
  ) {
  | Ok(updatedUser) => Some(#UserNameWasUpdated({"updatedUser": updatedUser}))
  | Error(Unauthorized) =>
    Some(#UserNameUpdateFailed({"reason": #NOT_AUTHORIZED}))
  | Error(_) => Some(#UserNameUpdateFailed({"reason": #UNKNOWN_ERROR}))
  }
}

```

This produces this schema:

```graphql
enum UserNameUpdateFailedReason {
  NOT_AUTHORIZED
  UNKNOWN_ERROR
}

type UserNameWasUpdated {
  updatedUser: User!
}

type UserNameUpdateFailed {
  reason: UserNameUpdateFailedReason!
}

union UserUpdateName = UserNameWasUpdated | UserNameUpdateFailed

type Mutation {
  userUpdateName(userId: ID!, newName: String!): UserUpdateName
}
```

There, we now have an inferred enum as well inside of our inferred union. Inference everywhere!

### Sometimes it makes more sense to not use inferred unions

Inferred unions can be used as return types from resolvers only. Each union will be named `<parentTypeName><fieldName>`, and optionally if it's on a mutation, be appended with `Result`.

Inferred unions certainly has their place, especially in mutations. But just as with inferred enums, there are downsides. They:

- Can't use doc strings
- Can't control the name of the generated type
- Can't reuse the type unless you declare it

Regardless of the downsides, inferred unions are really cool and useful. Use them when they make sense!

## Next steps

Now that we've covered unions, we can move on to [interfaces](interfaces).
