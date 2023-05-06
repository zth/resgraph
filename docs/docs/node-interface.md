# The Node Interface

> You're encouraged to read up on the Node interface before reading this text. Here's a [general article on the Node interface](https://dev.to/zth/the-magic-of-the-node-interface-4le1), and here's the relevant [best practices section from the GraphQL documentation](https://graphql.org/learn/global-object-identification/).

If you want to unlock the true power of GraphQL clients like Relay, you're strongly encouraged to implement what's called the Node interface.

### Node interface crash course

The Node interface is essentially a convention that lets each node in the graph:

1. Have an `id` that's unique in your _entire_ graph, not just among the type the node is of.
2. Be possible to refetch each node by itself only via its `id`. Meaning it needs to be possible to destruct `id` into something that reveals both the _actual_ `id`, and what type it's for, so we know what type to fetch.

Together, these two points enable clients like Relay to:

1. Do normalized caching effortlessly, which brings all sorts of goodies like data consistency, automatic cache updates and so on.
2. Automatically build queries for pagination and refetching, so you don't have to write those queries yourself.

In a nutshell, the Node interface looks and works something like this:

```graphql
# The schema

# Defines the Node interface
interface Node {
  id: ID!
}

# Each type implementing Node should have a globally unique ID
type User implements Node {
  id: ID!
  name: String!
}

# `Query` should have a top level field called `node` that lets you refetch any node in the graph by its id
type Query {
  node(id: ID!): Node
}
```

Now, imagine we've got an `id` for a `User` from somewhere in our graph. Without having to care about _where_ that `User` came from, we can fetch data from that exact user via the Node interface:

```graphql
query NodeTestQuery($userId: ID!) {
  node(id: $userId) {
    ... on User {
      name
    }
  }
}
```

This would return data from `User` if the value we pass to `userId` is indeed an `id` coming from a `User`.

## The Node interface in ResGraph

Now, all of the above sounds great. But it's up to you to implement all of that in practice. From our long experience of building GraphQL servers we know that this can feel and be much trickier than it needs to be. So, let us walk you through how easy implementing the Node interface is in ResGraph.

In a nutshell, what we need to do is the following:

1. Define our `Node` interface type.
2. Expose a field called `id` from that interface. That `id` needs to be an `ID` in GraphQL, and needs to resolve as a globally unique ID for each type that implements it.
3. Expose a `node` field on `Query` that takes an `id` and can resolve each of the types that implement the `Node` interface.

Let's get started!

### 1. Defining the Node interface

Start by defining the Node interface. The Node interface is a convention, so it's important that it's named `node`, even though there's nothing _actually_ magical about that name:

```rescript
// NodeInterface.res
@gql.interface
type node = {
  id: string
}
```

Notice we don't expose `id` as a GraphQL field directly, nor do we type it as `ResGraph.id`. More on why in the next step.

### 2. Implementing the globally unique `id` field for each type

Now we need to make sure that each and every type that implements `Node` has an `id` field that returns a _globally unique id_. We'll do this step by step. Let's start by defining a field `id` for the interface `Node`:

```rescript
// NodeInterfaceResolvers.res
@gql.field
let id = (node: NodeInterface.node) => {
  node.id->ResGraph.id
}
```

This adds an `id: ID!` field to all types that implement `Node`. Great! But this is just returning the `id` coming from the type itself, so it's not going to be globally unique by default.

One "naive" idea would be to have a separate `id` field function for each type to produce the globally unique ID:

```rescript
@gql.field
let id = (user: user) => {
  `User:${user.id}`->ResGraph.id
}
```

But that will get old fast as we add new types and need to add new field functions just for the ID every time.

Instead, we can leverage [accessing what type the interface field function is currently working on](interfaces#accessing-what-type-the-interface-field-function-is-currently-working-on). That, together with helpers that ResGraph generates for us, means we can build our generic node `id` field in a way that'll work for all types that implement it automatically, without having to roll separate field functions for each type:

```rescript
// NodeInterfaceResolvers.res
@gql.field
let id = (node: NodeInterface.node, ~typename: ResGraphSchemaAssets.node_implementedBy) => {
  `${typename->ResGraphSchemaAssets.node_typenameToString}:${node.id}`->ResGraph.id
}
```

There! Now every `id` field of every type that implements `Node` will be globally unique, and in the form of `<type>:<id>`. So, for a user with the ID `123`, the globally unique ID will automatically be generated as `User:123`.

### Expose a `node` field on `Query`

The final thing we need to do before we have a fully working Node interface setup is to implement the `node` field on `Query`, that takes any `id` from a node and resolves its type.

Fortunately, ResGraph generates helpers for this task as well. Let's look at how an implementation of the `node` field can look:

```rescript
/** Fetches an object given its ID.*/
@gql.field
let node = async (_: Query.query, ~id, ~ctx: ResGraphContext.context): option<
  ResGraphSchemaAssets.node_resolver,
> => {
  switch id->ResGraph.idToString->String.split(":") {
  | [typenameAsString, id] =>
    switch typenameAsString->ResGraphSchemaAssets.decodeImplementedByInterface_node {
    | None => None
    | Some(User) =>
      switch await ctx.dataLoaders.userById.load(~userId=id) {
      | Ok(user) => Some(User(user))
      | Error(_) => None
      }
    | Some(Group) =>
      switch await ctx.dataLoaders.groupById.load(~userId=id) {
      | Ok(group) => Some(Group(group))
      | Error(_) => None
      }
    }
  | _ => None
  }
}
```

Lots of things to distill here.

1. We annotate the return type of our field function as `ResGraphSchemaAssets.node_resolver`. This type is autogenerated, and is what ensures that you can return only types that implement `Node`. It also informs ResGraph that it's indeed the _interface_ `Node` you're returning from this field.
2. Remember that we formatted our IDs like `<type>:<id>`. Because of that, we start by decoding the raw id passed to `node` by splitting the `id` on `:` to extract our type and real id.
3. We then use `ResGraphSchemaAssets.decodeImplementedByInterface_node` to decode that string into a `ResGraphSchemaAssets.node_implementedBy` type. These are both autogenerated by ResGraph, and will stay up to date with any types that start (or stop) implementing the Node interface. `node_implementedBy` is a variant listing all typenames that implement `Node`, and `decodeImplementedByInterface_node` is a function that decodes a string into a `node_implementedBy`.
4. Now we have a type, and an id. We can move on to resolving the correct nodes.

There! Now we have a `node` field that can resolve any node implementing the `Node` interface by itself.

## Advanced patterns

So far we've done an as simple implementation of our IDs as possible. However, there are several more things you might want to consider. In this section we'll discuss those considerations a bit.

### Obfuscating the `id`

One thing you can consider is to "obfuscate" the `id` so it doesn't read `<type>:<id>` if you inspect it at runtime. Doing this can be for several reasons:

1. `id` should be considered opaque, as in the client should not be tempted to rely on its implementation, because the server owns it and should be allowed to change it at any time. Obfuscating the `id` so it's not directly human readable is a simple way to communicate to the client that "this is intended to be opaque to you".
2. You might want to stick the `id` into links and URL:s, meaning they'll need to be URL safe and generally look like an id you're not tempted to tamper with in the URL.

The most common way in GraphQL to handle this is to encode the `id` as (URL safe) base64. Let's look at how you can do that easily with ResGraph.

### Base64 encoding `id`

**TODO: Finish as we've merged the connection branch**
