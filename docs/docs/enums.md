---
sidebar_position: 3
---

# Enums

Enums are defined by defining a variant without payloads and annotate it with `@gql.enum`:

```rescript
@gql.enum
type userStatus = Online | Offline
```

```graphql
enum UserStatus {
  Online
  Offline
}
```

Notice enums _must_ be variants without payloads, or ResGraph will complain.

## Using enums in the schema

Enums are valid to use anywhere in your schema where they are valid in GraphQL. ResGraph just needs to understand that it's your particular enum it's looking for. A few examples:

```rescript
@gql.enum
type userStatus = Online | Offline

@gql.type
type user = {
  id: string,

  @gql.field
  currentStatus: option<userStatus>
}

@gql.field
let lastKnownStatus = (user: user, ~ctx: ResGraphContext.context) => {
  // Returns promise<option<userStatus>>
  ctx.dataLoaders.user.lastKnownStatus.load(~userId=user.id)
}

@gql.field
let currentUsersWithStatus = (_: query, ~status: userStatus, ~ctx: ResGraphContext.context) => {
  // Returns promise<array<user>>
  ctx.dataLoaders.user.byCurrentStatus.load(~status)
}
```

This above will generate the following GraphQL:

```graphql
enum UserStatus {
  Online
  Offline
}

type User {
  currentStatus: UserStatus
  lastKnownStatus: UserStatus
}

type Query {
  currentUsersWithStatus: [User!]!
}
```

## Comments and deprecations

You can add comments to the enum definition, and to each enum case. You can also add deprecations to each enum case via the `@deprecated` attribute. Here's a full example demonstrating all of the above:

```rescript
/** The status of the user.*/
@gql.enum
type userStatus =
  | /** The user is online. */ Online
  | /** The user is offline. */ Offline
  | /** The user is idle. */ @deprecated("This is going away") Idle
```

```graphql
"""
The status of the user.
"""
enum UserStatus {
  """
  The user is online.
  """
  Online
  """
  The user is offline.
  """
  Offline
  """
  The user is idle.
  """
  Idle @deprecated(reason: "This is going away")
}
```

## Customizing enum case values

Sometimes you want to customize the value of an enum case for various reasons, in a way that might not be possible given ReScript's rules around naming variant cases. For that, you can use the `@as` attribute together with a string literal:

```rescript
@gql.enum
type userStatus =
  | @as("ONLINE") Online
  | @as("OFFLINE") Offline
```

```graphql
enum UserStatus {
  ONLINE
  OFFLINE
}
```

## Advanced: Inferred enums

The strategies outlined above are the most efficient and best ways of using enums in ResGraph. However, there are scenarios, like when prototyping or working on something isolated that won't be reused, where you might now want to have to write out your enum type definition by hand. In these cases, you can _infer your enums from polyvariants_.

Remember [polymorphic variants](https://rescript-lang.org/docs/manual/latest/polymorphic-variant) in ReScript? A cousin of regular variants, with some drawbacks and some advantages compared to regular variants. One of the advantages is that polyvariants can be _inferred_ from your code usage. And ResGraph can leverage this inference to produce enums automatically for your GraphQL schema.

Let's look at an example:

```rescript
let displayName = (user: user, ~format) => {
  switch format {
    | Some(#Long) => `${user.firstName} ${user.lastName}`
    | Some(#Short) => user.firstName
    | Some(#Initials) => getInitials(user)
    | None => `${user.firstName} ${user.lastName}`
  }
}
```

This produces this schema:

```graphql
enum UserDisplayNameFormat {
  Long
  Short
  Initials
}

type User {
  displayName(format: UserDisplayNameFormat): String!
}
```

Notice there's no type annotation for the `format` argument. It's all inferred by ReScript from your usage in the code. And from that, ResGraph can infer a enum for you. This works in the following places:

- As arguments
- As return types
- Nested inside of other inferred objects (regular and input objects)

This is really neat for rapidly prototyping things, and there certainly are "build-for-a-single-use" scenarios where this makes sense. Each inferred enum will be automatically named according to its context. For arguments, it'll be named `<parentTypeName><fieldName><argumentName>`. For return types `<parentTypeName><fieldName>`, and so on.

You're advised to _use this to work quickly_, but then solidify your enum into an actual declared `@gql.enum` when you're done working, unless you really don't need the benefits of a "real" enum. Here are the downsides of using an inferred enum compared to an actual declared one:

- Can't use doc strings
- Can't control runtime representation with `@as`
- Can't control the name of the generated type
- Can't reuse the type unless you declare it (at which point you might as well just make a regular `@gql.enum`)

Still, inferred enums are really cool! Use them to work quickly without type declarations needing to get into your way.

## Next steps

Let's look at how enum's close friend [unions are defined](unions) in ResGraph.
