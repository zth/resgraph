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

Let's look at how enum's close friend [unions are defined](unions) in ResGraph.
