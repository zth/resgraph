# Pagination

> You're encouraged to read up on connections in GraphQL prior to reading this section. Here's [an article](https://dev.to/zth/connection-based-pagination-in-graphql-2588), and there's a link to the [official GraphQL documentation on pagination](https://graphql.org/learn/pagination) which explains the rationale for using connections well.

The best practice for doing pagination in GraphQL is leveraging a concept called connections. Connections are an integral part of building a GraphQL server following best practices. ResGraph comes with a number of conveniences to make working with connections simpler. Let's dive into what they are and how to use them.

> Throughout this text, remember that while ResGraph ships with helpers for connections, _connections are nothing but a specification_. You can create your own hand rolled connections by just defining types, you don't have to use what ResGraph ships. It's just there for convenience.

## Setting up a simple connection

ResGraph comes with pre-defined type creators (TODO: Link) for easily creating new connections:

```rescript
@gql.type
type user = {
  id: string,
  @gql.field name: string
}

/** An edge to a user. */
@gql.type
type userEdge = ResGraph.Connections.edge<user>

/** A connection to users. */
@gql.type
type userConnection = ResGraph.Connections.connection<userEdge>
```

There, we have the simplest possible connection set up, by using the built-in `ResGraph.Connections.edge<'node>` and `ResGraph.Connections.node<'edge>`. This will create a connection type and an edge type hooked up to that connection type. It generates this GraphQL:

```graphql
"""
A connection to users.
"""
type UserConnection {
  """
  Information to aid in pagination.
  """
  pageInfo: PageInfo!

  """
  A list of edges.
  """
  edges: [UserEdge]
}

"""
An edge to a user.
"""
type UserEdge {
  """
  The item at the end of the edge.
  """
  node: User

  """
  A cursor for use in pagination.
  """
  cursor: String!
}
```

You can then return this connection from a field just like you'd return any type.

## Helpers for constructing connections

When doing real connection based pagination on the backend you'll naturally fill in the full connection yourself as you return it from fields.

But, it can be useful to present list data to the client as a connection even if what you have on the backend isn't a real connection. For this, ResGraph ships with a few helpers incorporated from [`graphql-relay`](https://github.com/graphql/graphql-relay-js).

Let's look at how we can leverage those helpers to return a connection even if all we have on the backend is a list of `user` and not a real connection:

```rescript
@gql.type
type user = {
  id: string,
  @gql.field name: string,
}

/** An edge to a user. */
@gql.type
type userEdge = ResGraph.Connections.edge<user>

/** A connection to users. */
@gql.type
type userConnection = ResGraph.Connections.connection<userEdge>

/** All currently active users. */
@gql.field
let currentlyActiveUsers = (
  _: query,
  ~ctx: ResGraphContext.context,
  ~first,
  ~after,
  ~before,
  ~last,
): userConnection => {
  // Returns array<user>
  let activeUsers = await ctx.dataLoaders.activeUsers.load()

  activeUsers->ResGraph.Connections.connectionFromArray(
    ~args={first, after, before, last},
  )
}
```

And this will generate the following GraphQL:

```graphql
"""
A connection to users.
"""
type UserConnection {
  """
  Information to aid in pagination.
  """
  pageInfo: User!

  """
  A list of edges.
  """
  edges: [UserEdge]
}

"""
An edge to a user.
"""
type UserEdge {
  """
  The item at the end of the edge.
  """
  node: User

  """
  A cursor for use in pagination.
  """
  cursor: String!
}

type Query {
  """
  All currently active users.
  """
  currentlyActiveUsers(
    first: Int
    after: String
    before: String
    last: Int
  ): UserConnection!
}
```

Let's look a bit deeper into what we did, and how it works:

1. First notice we're adding all connection arguments (`first`/`last`/`before`/`after`) to our field function. Also notice we're not annotating them with types - let inference do its thing! `args` of `connectionFromArray` knows what types they should be, so we let ReScript infer that for us.
2. Also notice that we're annotating the return type of the function with `userConnection`. This is important when using the `connectionFromArray` helper, because it returns a _generic_ connection, and ResGraph won't understand that that generic connection is in fact `userConnection` unless we tell it.
3. We then call a fictive data loader that returns an array of `user`. Notice it returns an array, and not a connection.
4. Finally, we use `connectionFromArray` to turn our array of `user` into a `connection<user>`, which is the same as `userConnection`.

That last part may seem a bit magical, so we'll briefly dig into how `connectionFromArray` works next.

### `connectionFromArray`

This helper will create a _synthetic_ connection from an array of items. It does that by simply slicing the array according to the arguments you give it, and keep track of _where_ in the array it's currently at by using the item indexes of the array as cursors.

It's a simple and fast way of exposing array-based data as a connection to the frontend, so that the frontend can easily choose how much data to fetch, and paginate that data, even if the backend practically does not support pagination for that array of items.

For this to work, the array you leverage needs to be _static_ (as in it doesn't change frequently). This is important, because if the array changes, you'll end up getting the wrong items in the client as you paginate.

Use this helper with caution. It's typically a good idea to use when you want to expose a list of something as a connection, preferably where it's not intended to be paginated a lot. If you intend to also paginate that list a lot, make it a real connection on the backend instead.

## Extending the connection

Edges and connections are regular [object types](object-types), so you can add fields to your connection (and your edges) by defining [field functions via `@gql.field`](object-types#adding-fields-to-types-via-functions), just like with any other object type.

If you find yourself wanting to change the documentation for a field in the generated connection, you can easily do so with a field function as well, that just returns the underlying data directly:

```rescript
/** The user for this edge. */
@gql.field
let node = (edge: userEdge) => {
  edge.node
}
```

Generates:

```graphql
"""
An edge to a user.
"""
type UserEdge {
  """
  The user for this edge.
  """
  node: User

  """
  A cursor for use in pagination.
  """
  cursor: String!
}
```

## Adding more data to the connection

Sometimes you'll want to expose fields on the connection that requires more data than what the generic connection shape allows for. For those instances, there's a sibling to `ResGraph.Connections.connection<'edge>` called `ResGraph.Connections.connectionWithExtra<'edge, 'extra>`. Using that connection type creator allows you to add data you can access in the connection via a field `extra: 'extra`. Let's look at an example:

```rescript
@gql.type
type userEdge = ResGraph.Connections.edge<user>

type connectionExtra = {totalCountFromServer: int}

@gql.type
type userConnection = ResGraph.Connections.connectionWithExtra<userEdge, connectionExtra>
```

Here we define our `userConnection` with `connectionWithExtra`, which allows us to attach extra data to the connection.

With this, we can expose a field returning `totalCountFromServer` easily:

```rescript
/** The total amount of data available on the server. */
@gql.field
let totalCount = (connection: userConnection) => {
  Some(connection.extra.totalCountFromServer)
}
```

Finally, whenever constructing this connection we'll use `connectionFromArrayWithExtra` instead of `connectionFromArray` to create our connection:

```rescript
/** All currently active users. */
@gql.field
let currentlyActiveUsers = (
  _: query,
  ~ctx: ResGraphContext.context,
  ~first,
  ~after,
  ~before,
  ~last,
): userConnection => {
  // Returns {activeUsers: array<user>, totalCount: int}
  let {activeUsers, totalCount} = await ctx.dataLoaders.activeUsers.load()

  activeUsers->ResGraph.Connections.connectionFromArrayWithExtra(
    ~args={first, after, before, last},
    ~extra={totalCountFromServer: totalCount}
  )
}
```

There, we've now added another field to our (generic) connection, that's backed by extra data added to the connection. This results in the following GraphQL:

```graphql
"""
A connection to users.
"""
type UserConnection {
  """
  The total amount of data available on the server.
  """
  totalCount: Int

  """
  Information to aid in pagination.
  """
  pageInfo: PageInfo!

  """
  A list of edges.
  """
  edges: [UserEdge]
}

"""
An edge to a user.
"""
type UserEdge {
  """
  The item at the end of the edge.
  """
  node: User

  """
  A cursor for use in pagination.
  """
  cursor: String!
}

type Query {
  """
  All currently active users.
  """
  currentlyActiveUsers(
    first: Int
    after: String
    before: String
    last: Int
  ): UserConnection!
}
```

## Rolling your own connection entirely

The helpers presented above are intended to solve the _basic_ cases. If you have more advanced cases, like adding fields that can't be derived from the basic data to the edges of a connection, you're encouraged to just roll your own connection definition and construct the connection data by hand, leveraging the connection arguments `first`/`last`/`before`/`after` provided to you, and so on.

It's as easy as defining your own `@gql.type` for the edge and connection. Here's an example:

```rescript
@gql.type
/** An edge in a connection. */
type userEdge = {
  /** A cursor for use in pagination. */
  @gql.field
  cursor: string,
  /** The item at the end of the edge. */
  @gql.field
  node: option<user>
}

/** A connection to a list of items. */
@gql.type
type userConnection = {
  /** Information to aid in pagination. */
  @gql.field
  pageInfo: ResGraph.Connections.pageInfo,
  /** A list of edges. */
  @gql.field
  edges: option<array<option<userEdge>>>
}
```

You'll need to implement the logic for constructing and paginating the connection yourself, but you can leverage inference and the `ResGraph.Connections.connectionArgs` type to help you take the correct connection arguments for your field:

```rescript
/** Friends of the user. */
@gql.field
let friends = (
  user: user,
  ~ctx: ResGraphContext.context,
  ~first,
  ~after,
  ~last,
  ~before,
) => {
  // Fictive loader that returns a complete `userConnection`.
  // (~userId: string, ~args: ResGraph.Connections.connectionArgs) => promise<userConnection>
  let userFriends = await ctx.dataLoaders.user.friends.load(
    ~userId=user.id,
    ~args={
      first,
      after,
      last,
      before,
    },
  )

  Some(userFriends)
}
```

Here our data loader takes `ResGraph.Connections.connectionArgs`, which again let us not have to worry about the types of the args `first`/`after`/`before`/`last`, thanks to inference.
