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

### Customizing record-backed field names with `@as`

Sometimes the GraphQL field name you need is awkward or impossible to use as a normal ReScript record field name. For example, `type`, `constraint`, `module` and other reserved ReScript keywords are valid GraphQL field names, but they cannot be used as ordinary ReScript identifiers.

For record-backed fields, use ReScript's `@as` attribute together with `@gql.field`. This keeps the ReScript field name usable while emitting the `@as` name in GraphQL:

```rescript
@gql.type
type rule = {
  @as("constraint") @gql.field
  constraint_: string,
  @as("type") @gql.field
  type_: string,
}
```

This emits the `@as` names in the GraphQL schema:

```graphql
type Rule {
  constraint: String!
  type: String!
}
```

When constructing the record in ReScript, use the ReScript field names:

```rescript
let rule = {
  constraint_: "required",
  type_: "validation",
}
```

ResGraph validates the final GraphQL field names after applying `@as`, so two fields cannot accidentally emit the same GraphQL name.

Use `@as` for record-backed fields, including fields on `@gql.type` records, `@gql.interface` records, and inline record payloads in unions. Input object fields use the same pattern, covered in [input objects](input-objects#customizing-field-names-with-as).

`@as` does not rename resolver arguments. For reserved argument names, use escaped labels as shown in [adding arguments to your fields](#adding-arguments-to-your-fields).

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

### Defining root fields without a source argument

Root fields can omit the otherwise-unused source argument by using
`@gql.query`, `@gql.mutation`, or `@gql.subscription`:

```rescript
@gql.query
let greeting = () => "hello"

@gql.query
let search = (~term: string, ~ctx: ResGraphContext.context) =>
  Search.find(~term, ~ctx)

@gql.mutation
let increment = (~value: int) => value + 1
```

The root type is synthesized when it has not been declared explicitly. A
zero-argument root resolver takes `unit`; a resolver with inputs can use only
labelled arguments. Defaults, descriptions, deprecations, directives, context,
`resolveInfo`, authorization, and subscription return validation work exactly
as they do for `@gql.field` resolvers.

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
  fullName(includeInitials: Boolean = false): String
}
```

The ReScript default expression is the GraphQL default and must be a GraphQL
constant. ResGraph validates it against the inferred argument type and the
generated resolver receives the coerced value.

Parameter attributes add the rest of GraphQL's argument metadata:

```rescript
@gql.field
let users = (
  _: query,
  @gql.description("Maximum number of users to return.")
  @gql.annotate({name: "cost", args: {credits: 2}})
  @deprecated("Use pageSize instead.")
  ~limit: int=20,
) => {
  loadUsers(~limit)
}
```

This emits the description, default, deprecation, and directive on the
`Query.users(limit:)` argument. ReScript does not accept doc comments directly
on function parameters, so argument descriptions use `@gql.description`.

Arguments can also be [input objects](input-objects), [custom scalars](custom-scalars) and so on.

If an argument needs a GraphQL name that is reserved in ReScript, use ReScript's escaped label syntax and bind it to a local name:

```rescript
@gql.field
let check = (_: query, ~\"constraint" as constraint_: string) => constraint_
```

This emits the argument as `constraint` in GraphQL while letting the resolver body use `constraint_`.

Use this for arguments on any `@gql.field` function: root fields on `Query`, `Mutation`, and `Subscription`, fields added to object types, and fields added to interfaces.

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
