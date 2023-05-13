# ResGraph Changelog

## main

### New Features

- [LSP] Add autocompletion for `ctx: ResGraphContext.context` to make using context easier.
- [LSP] Context aware snippets for inserting new query fields and full mutations.
- [LSP] Support showing info about the backing ReScript code when hovering in `schema.graphql`.
- [LSP] Support jump-to-definition to the backing ReScript code from items in `schema.graphql`.

### Bug fix

- Print variants properly in debug SDL.

## 0.4.0

### New Features

- Inline records are now allowed on unions. Using inline records will create a "synthetic" type for that particular payload.

### Bug fix

- Variant constructor names for union members are now allowed to be anything.

## 0.3.0

### New Features

- Ship (opinionated) bindings to `DataLoader` by default. https://github.com/zth/resgraph/pull/21

### Bug fix

- Ensure build is triggered directly when starting watch mode.
- Print errors in watch mode.

## 0.2.0

### New Features

- Hovering on `@gql` types now show what GraphQL they yield directly in the hover. https://github.com/zth/resgraph/pull/12
- Allow injecting current `typename` for "general" field functions on interfaces. https://github.com/zth/resgraph/pull/9
- Add snippets for boilerplate for connections and field functions. https://github.com/zth/resgraph/pull/8

### Bug fix

- Ensure schema SDL file is dumped also on build by default. This will eventually be configurable.
- Fix bug where some interface spreads wouldn't be picked up if the type definition had single line comments.

## 0.1.3

### Bug fix

- Escape descriptions in GraphQL schema.

## 0.1.2

### Bug fix

- Include full ReScript src dir.

## 0.1.1

### Bug fix

- Fix issue calling generated binary (that didn't account for the bin location properly).

## 0.1.0

Initial release!
