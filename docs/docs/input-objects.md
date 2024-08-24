---
sidebar_position: 7
---

# Input Objects

Input objects are defined by using a record annotated with `@gql.inputObject`:

```rescript
/** Config for searching for a user. */
@gql.inputObject
type userSearchConfig = {
  groupId?: ResGraph.id,
  name?: string,
}

@gql.field
let searchForUser = (_: query, ~input: userConfig, ~ctx: ResGraphContext.context): option<user> => {
  ctx.dataLoaders.searchForUser.load(~name=input.name, ~groupId=input.groupId)
}
```

```graphql
input UserSearchConfig {
  groupId: ID
  name: String
}

type Query {
  searchForUser(input: UserSearchConfig!): User
}
```

One big difference compared to [object type fields](object-types#fields) is that in input objects, all fields are automatically exposed, because the contrary wouldn't make sense since this is an input from the client and not something you're supposed to construct yourself.

### Comments

You can add comments to the type definition itself, and to all record fields. These will then be exposed in your schema.

### Handling `null`

Just like in [arguments of object type fields](object-types#handling-null-in-arguments), you can choose to explicitly handle `null` values by annotating fields in the input object to be `Js.Nullable.t`.

### Recursive input objects

Input objects are allowed to be (mutually) recursive, if they're not recursive in a non-nullable way, as that would create an endless loop.

In ReScript, you'd handle that scenario like you'd normally handle mutually recursive types:

```rescript
/** Some user config. */
@gql.inputObject
type rec userConfig = {
  name?: string,
  additionalConfig?: userConfig,
  lastGroupConfig?: groupConfig,
}
@gql.inputObject /** Some group config. */
and groupConfig = {
  groupName: string,
  additionalUserConfig?: userConfig,
}
```

### Inferred input objects

ResGraph can infer input objects if you use a [ReScript object](https://rescript-lang.org/docs/manual/latest/object) as an argument. Remember that ReScript objects can be fully inferred from usage. This creates a great way to create an input object fully inferred from usage. Let's look at an example:

```rescript
let userUpdateName = (_: mutation, ~input, ~ctx: ResGraphContext.context) => {
  switch await ctx.db.user.updateStatus(
    ~userId=input["userId"]->ResGraph.idToString,
    ~newName=input["newName"],
  ) {
  | Ok(updatedUser) => Some(updatedUser)
  | Error(_) => None
  }
}

```

This would produce this schema:

```graphql
input UserUpdateNameInput {
  userId: ID!
  newName: String!
}

type Mutation {
  userUpdateName(input: UserUpdateNameInput!): User
}
```

Notice everything is inferred from usage, because ReScript infers `input` as an object. And ResGraph leverages that to create an input object. Inferred input objects are named `<parentTypeName><fieldName><argumentName>`, and can only appear in the argument position of a resolver.

#### When to use and not to use inferred input objects

Inferred input objects can be great for moving quickly, but they have the following downsides:

- Can't use doc strings
- Can't be reused
- Can't be pattern matched like a record can

So, prefer regular declared `@gql.inputObject`, and use inferred input objects when you're protoyping or otherwise want to move as quickly as possible, or when you're doing something that's truly ad hoc and doesn't require documentation.

### Input unions

Often you'll find yourself in a scenario where you want to your input to be _one of_ several different values. For this, ResGraph has first class support for [input unions](input-unions).
