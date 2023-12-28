---
sidebar_position: 2
---

# Object Types

[GraphQL object types](https://graphql.org/learn/schema/#object-types-and-fields) are defined using the annotation `@gql.type` on a ReScript record:

```rescript
/** A user in the system. */
@gql.type
type user = {
  age: int,
  lastName: string,
  /** The first name of the user. */
  @gql.field
  firstName: string,

}

```

```graphql
"""
A user in the system.
"""
type User {
  """
  The first name of the user.
  """
  firstName: String!
}
```

Notice a few things here:

- Comments on types and fields end up in the GraphQL schema.
- You can expose fields directly from the type by annotating a record field with `@gql.field`. This is useful when you don't need to do any transformation of the underlying data before you return it to GraphQL.

## Fields

Building on point 2 above, there are two ways to add fields to a type. One is annotating fields with `@gql.field`. This will expose them as-is directly in the schema, meaning the raw value will be returned.

The second way is to define a field on a type via a function. What's commonly called a _resolver_ in GraphQL.

### Adding fields to types via functions

You can add a field to a GraphQL type this way:

```rescript
/** The full name of the user. */
@gql.field
let fullName = (user: user) => {
  Some(`${user.firstName} ${user.lastName}`)
}
```

This will expose `fullName` as a field on `User`:

```graphql
"""
A user in the system.
"""
type User {
  """
  The first name of the user.
  """
  firstName: String!

  """
  The full name of the user.
  """
  fullName: String
}
```

Notice a few things:

1. The function takes a `user` as the first, unlabelled argument. This is how ResGraph figures out what type this field should be attached to.
2. Comments work just like you'd expect, and they end up in the schema.
3. We don't annotate the return type of the function, but we could if we'd like to.
4. This is a sync function, but it could just as well be an `async` function. ResGraph handles both.

This is likely going to be the main way you add fields to your object types. Let's dive in to how to do a few more things:

### Adding arguments to your fields

Using arguments for your field is as easy as adding a labelled argument to your function:

```rescript
/** The full name of the user. */
@gql.field
let fullName = (user: user, ~includeInitials=false) => {
  let initials = if includeInitials {
    ` (${getFirstCharUppercased(user.firstName)} ${getFirstCharUppercased(user.lastName)})`
  } else {
    ""
  }

  Some(`${user.firstName} ${user.lastName}${initials}`)
}
```

This will add the argument `includeInitials` to your field.

```graphql
"""
A user in the system.
"""
type User {
  """
  The first name of the user.
  """
  firstName: String!

  """
  The full name of the user.
  """
  fullName(includeInitials: Boolean): String
}
```

Arguments can also be [input objects](input-objects), [custom scalars](custom-scalars) and so on.

> Note: Anything exposed to GraphQL, like fields, arguments and so on, must all be [valid GraphQL types](valid-graphql-types). ResGraph will complain (and tell you how to fix it) if you try and use anything not valid.

#### Handling `null` in arguments

By default, all optional arguments are collapsed into a ReScript `option`. But, arguments can be explicitly set to `null` from the client in GraphQL. So, by default, whether the argument value was indeed `null` or just not set, is lost. This is OK for the vast majority of cases, but there _are_ cases when you do want to know whether some argument was explicitly `null`.

To solve that, just ensure your argument is of type `Js.Nullable.t` (TODO: Core `Nullable.t` instead). For any argument that's annotated as (or inferred to be) `Js.Nullable.t`, ResGraph will preserve `null` values.

Let's look at an example:

```rescript
@gql.field
let wasNull = async (_: query, ~blogPostId: Js.Nullable.t<ResGraph.id>) => {
  switch blogPostId {
  | Null => "Value was null"
  | Undefined => "Value was undefined"
  | Value(blogPostId) => "Id was: " ++ blogPostId->ResGraph.idToString
  }
}
```

```graphql
type Query {
  wasNull(blogPostId: ID): String!
}
```

### Deprecating fields

Deprecate fields via the `@deprecated` attribute:

```rescript
@gql.type
type user = {
  age: int,
  lastName: string,
  @gql.field @deprecated("This is going away, use 'fullName' instead.")
  firstName: string,

}

```

```graphql
type User {
  firstName: String!
    @deprecated(reason: "This is going away, use 'fullName' instead.")
}
```

### Using app context in field functions

To use the app context in your field functions, add a labelled argument annotated with `ResGraphContext.context` (the context you've created and defined) and ResGraph will inject your app context into that argument for your field:

```rescript
/** The full name of the user. */
@gql.field
let fullName = (user: user, ~includeInitials=false, ~ctx: ResGraphContext.context) => {
  let initials = if includeInitials {
    // Imagine we've added `utils.nameToInitials` to our context
    ctx.utils.nameToInitials(user.firstName, user.lastName)
  } else {
    ""
  }

  Some(`${user.firstName} ${user.lastName}${initials}`)
}
```

This is going to be where you use data loaders and other per-request contextual helpers from.

## Accessing `GraphQLResolveInfo` for each resolver

Similarly to accessing the context, you can get access to the `resolveInfo` argument for each resolver (typed as `GraphQLResolveInfo`) by adding a labelled argument annotated with `ResGraph.resolveInfo`:

```rescript
/** The full name of the user. */
@gql.field
let fullName = (user: user, ~info: ResGraph.resolveInfo) => {
  Console.log(info)

  Some("Test User")
}
```

> Note: The `resolveInfo` type is currently not complete. It'll be extended in the future to give you access to all the information in `info` directly, but for now you can write your own bindings for the things in `GraphQLResolveInfo` that you need, and then just cast `resolveInfo` to that.
