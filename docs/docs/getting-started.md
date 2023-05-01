---
sidebar_position: 1
---

# Getting Started

> Note that ResGraph is currently _alpha grade software_. Help us out as we work out the details for a stable release.

## Setup

Let's start by setting up ResGraph in your project.

### Installation

ResGraph relies on features in ReScript `v11`, which is currently in alpha. Make sure you run `>= rescript@11.0.0-alpha.5` and for the best results, set `"uncurried": true` in your `bsconfig.json`.

```bash
# Install both `graphql` and `graphql-yoga` so we can set your server up
npm i resgraph graphql graphql-yoga @rescript/core @glennsl/rescript-fetch
```

Add `resgraph` to your `bs-dependencies` in `bsconfig.json`:

```json
{
  "uncurried": true,
  "bs-dependencies": ["resgraph", "@rescript/core", "@glennsl/rescript-fetch"]
}
```

### `resgraph.json`

Add a `resgraph.json` file in the root of your ReScript project, and paste this into it:

```json
{
  "src": "./src",
  "outputFolder": "./src/schema/__generated__"
}
```

- `src` is the folder where code that can define GraphQL lives
- `outputFolder` is where you want ResGraph to output the files it generates. **Ensure that this folder exists**. Create it if you don't already have it.

### `ResGraphContext.res`

Create a `ResGraphContext.res` file anywhere in your project, and add a `context` type in there. The `context` can have anything you'd like in it:

```rescript
// ResGraphContext.res
type context = {currentUserId: option<string>}
```

This is the type of your [per-request GraphQL context](object-types#using-app-context-in-field-functions), and ResGraph will ensure you fill out and provide this to every request as you create your server.

### Define a query type

Somewehere in your project, define a GraphQL [`Query`](query) type, and a field for it. This is the bare minimum to get a GraphQL server going.

```rescript
// Schema.res - Called `Schema` here, but this can be called anything
@gql.type
type query

/** The current time on the server, as a timestamp. */
@gql.field
let currentTime = (_: query) => {
  Some(Date.now())
}
```

> The root `Query` type is mandatory in a GraphQL schema, and is the base from where all queries will be made.

Make sure ReScript has built your changes (`npx rescript build`). Then run `npx resgraph build`. This should generate your first schema, looking like this:

```graphql
type Query {
  """
  The current time on the server, as a timestamp.
  """
  currentTime: Float
}
```

ResGraph will automatically generate 3 things by default:

1. The `ResGraphSchema.res` and `ResGraphSchema.resi` files. These hold the optimized `graphql-js` schema generated from your types and resolvers, that you then expose through your server.
2. `ResGraphSchemaAssets.res`. This holds various generated helpers for your schema. More on that file later.
3. `schema.graphql`. This is a schema SDL file dumped from ResGraph. This is intended to be used as a debug utility primarily.

#### Build and watch mode

Notice we built our schema using the one-shot command `resgraph build`. If you want to watch for changes and automatically rerun ResGraph, run `resgraph watch`. However, if you're also using VSCode, you're encouraged to instead use the [ResGraph VSCode extension](getting-started), which will run ResGraph in watch mode for you, show you errors and so on, directly inside of VSCode.

Excellent, we now have a schema! Let's hook it up to your server.

### Hooking up your schema to GraphQL Yoga

> ResGraph ships with GraphQL Yoga bindings, and we're using it to expose your schema. This is because Yoga is well maintained and well established in the ecosystem. However, hooking up ResGraph to other servers should be simple as well, since the primary artifact of ResGraph is just a `graphql-js` schema.

Let's expose your schema through GraphQL Yoga. Paste this into `App.res`:

```rescript
// App.res
open GraphQLYoga

let yoga = createYoga({
  schema: ResGraphSchema.schema,
  context: async ({request}) => {
    open ResGraphContext

    {
      currentUserId: request
        ->Request.headers
        ->Headers.get("x-user-id"),
    }
  },
})

let server = NodeHttpServer.createServer(yoga)

let port = 4555

server->NodeHttpServer.listen(port, () => {
  Console.info(`Server is running on http://localhost:${port->Int.toString}/graphql`)
})
```

After this builds (`npx rescript build`, or just run the compiler in watch mode), you can go ahead and run `node src/App.mjs` and you have yourself the simplest possible server running.

Before we break down what we did and why, you can ensure everything works by going to [GraphiQL](http://localhost:4555/graphql) and trying this query:

```graphql
query {
  currentTime
}
```

...which should give you back the current timestamp.

Now, let's break down what we did and why:

```rescript
let yoga = createYoga({
  schema: ResGraphSchema.schema,
  context: async ({request}) => {
    open ResGraphContext

    {
      currentUserId: request
        ->Request.headers
        ->Headers.get("x-user-id"),
    }
  },
})
```

> Pulling out `currentUserId` is just to examplify that this is per-request context.

This creates your Yoga server by linking together your schema (from the generated file `ResGraphSchema`) to a function that produces your specific app context type.

We then create and start the http server exposing our schema:

```rescript
let server = NodeHttpServer.createServer(yoga)

let port = 3000

server->NodeHttpServer.listen(port, () => {
  Console.info(`Server is running on http://localhost:${port->Int.toString}/graphql`)
})
```

## How does ResGraph work

At a high level, ResGraph works like this:

- Scans your projects for `@gql` annotations
- Reads type information, transforms the types and values annotated with `@gql` into a `graphql-js` schema

ResGraph runs on the compiled artifacts ReScript's compiler produces. So, ResGraph needs to run _after_ the compiler runs. Don't worry though, ResGraph has it's own watcher and build process that's easy to use and handles all of this.

## Next steps

There! We're all set up and your server is running. We can now continue our ResGraph journey by talking about how to define the primary building block of GraphQL - [object types](object-types).
