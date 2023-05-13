# ResGraph

Build implementation-first GraphQL servers in ReScript, with first class Relay integration.

- [Check out the docs on getting started](https://zth.github.io/resgraph/docs/getting-started).
- [Use this template to get started quickly](https://github.com/zth/resgraph-template).
- [Install the dedicated VSCode extension](https://marketplace.visualstudio.com/items?itemName=GabrielNordeborn.vscode-resgraph)

## What it looks like

This ReScript code:

```rescript
@gql.type
type query

/** A timestamp. */
@gql.scalar
type timestamp = float

/** A thing with a name. */
@gql.interface
type hasName = {@gql.field name: string}

@gql.type
type user = {
  ...hasName,
  @gql.field /** When this user was created. */ createdAt: timestamp,
  @gql.field @deprecated("Use 'name' instead") fullName: string,
}

/** Format for text. */
@gql.enum
type textFormat = Uppercase | Lowercase | Capitalized

/** The user's initials, e.g 'Alice Smith' becomes 'AS'. */
@gql.field
let initials = (user: user, ~format=Uppercase) => {
  let initials = getInitials(user.name)

  switch format {
  | Uppercase | Capitalized => initials->String.toUpperCase
  | Lowercase => initials->String.toLowerCase
  }
}

/** The current time on the server. */
@gql.field
let currentTime = (_: query): timestamp => {
  Date.now()
}

/** The currently logged in user, if any. */
@gql.field
let loggedInUser = async (_: query, ~ctx: ResGraphContext.context): option<user> => {
  switch ctx.currentUserId {
  | None => None
  | Some(userId) => await ctx.dataLoaders.userById.load(~userId)
  }
}
```

Generates this schema:

```graphql
type Query {
  """
  The current time on the server.
  """
  currentTime: Timestamp!

  """
  The currently logged in user, if any.
  """
  loggedInUser: User
}

"""
A timestamp.
"""
scalar Timestamp

type User implements HasName {
  """
  When this user was created.
  """
  createdAt: Timestamp!
  fullName: String! @deprecated(reason: "Use 'name' instead")

  """
  The user's initials, e.g 'Alice Smith' becomes 'AS'.
  """
  initials(format: TextFormat): String!
  name: String!
}

"""
A thing with a name.
"""
interface HasName {
  name: String!
}

"""
Format for text.
"""
enum TextFormat {
  Uppercase
  Lowercase
  Capitalized
}
```

[Check out the docs on getting started](https://zth.github.io/resgraph/docs/getting-started).

## Introduction

ResGraph lets you build _implementation first_ GraphQL servers, where your types and code is the source of truth for the schema.

## Features

- [x] Query
- [x] Mutations
- [ ] Subscriptions
- [x] Context
- [x] Object types
- [x] Input types
- [x] Interfaces
- [x] Enums
- [x] Unions
- [x] Custom scalars (including custom serializers/parsers)
- [ ] Directives
- [ ] Relay helpers (coming very soon)

## Development notes

This is a heavy work-in-progess. The repo itself was originally forked from the ReScript editor tooling, which means that there's a ton of stuff lying around that aren't really used. This will be cleaned up later.

### Local development

First, set up OCaml.

```bash
# If you haven't created the switch, do it. OPAM(https://opam.ocaml.org)
opam switch 4.14.0 # can also create local switch with opam switch create . 4.14.0

# Install dev dependencies from OPAM
opam install . --deps-only

# For IDE support, install the OCaml language server
opam install ocaml-lsp-server
```

Then, install JS dependencies:

```bash
npm i
```

Now, you can build `ResGraph` and run its tests:

```bash
# You might need to run this twice the first time
make test
```

Accompanied with the tests is a simple test server that'll let you run GraphiQL to execute operations against the test schema. Start it by running:

```bash
npm run watch-testapp
```

This will start the test server and watch for changes so it's restarted with any change. You can now access GraphiQL at http://localhost:9898/graphql.
The test server will also dump an up to date `schema.graphql` on restart, so you'll be able to see how any changes you make affect the schema.

For the best experience, also run the ReScript compiler as a separate process:

```bash
npx rescript build -w
```

The workflow after this is setup is roughly:

1. Make changes to the test schema (primarily located in `Schema.res`)
2. Run `ResGraph` via `make test`
3. Test the changes in GraphiQL, or see them in `schema.graphql`

## High level overview of how ResGraph works

1. You write your types and code in ReScript, and annotate them with attributes to expose them via GraphQL.
2. ResGraph runs after the ReScript compiler has recompiled, and scans your project for all GraphQL-related things. It then extracts all relevant GraphQL things from the compiled type information.
3. What it finds is then transformed into an actual `graphql-js` schema that you can run.

ResGraph is _fast_, because it's leveraging the standard ReScript tooling, and using OCaml.

## Acknowledgements

This is a spiritual sibling project to [Grats](https://github.com/captbaritone/grats), from which this project derives most of its ideas, reimplemented in the ReScript ecosystem instead.
