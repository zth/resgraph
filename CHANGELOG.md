# ResGraph Changelog

## main

- _experimental_ Allow payloadless variant cases in input unions.

## 0.13.2

- Fix bug when input objects and input unions were mixed.

## 0.13.1

- Bundle `cli` so it doesn't interfere unecessarily with other ReScript things.

## 0.13.0

- Disallow inferring input objects. It's not worth the complexity right now.
- Add support for subscriptions.

## 0.12.0

- **Experimental**. Allow inferring polyvariants and objects to unions, enums, input objects and object types in certain cases.

## 0.11.0

- Emit all types into the generated `GraphQLSchema` explicity.
- Also allow `Null.t` to be used as is, in addition to `Nullable.t`.
- Don't validate the number of fields in types, let the GraphQL server do that instead.

## 0.10.0

- Up `graphql-yoga` to `5.3.0` and `graphql` package to `16.8.1`.

## 0.9.1

- Include `rescript.json` in package.

## 0.9.0

- Depend on ReScript v11.
- Ship `arm64` binary.

## 0.8.0

- Update/improve `graphql-yoga` bindings.
- Upgrade `@rescript/core`.
- Bind `GraphQLError`.
- Remove support for "type creators" since it's now natively supported in ReScript via type spreads.
- First class support for input unions.

## 0.7.0

- Support `rescript.json` in addition to `bsconfig.json`.

## 0.6.3

- Up to latest `rescript@next` and remove dependency on (now removed) legacy compiler Node bindings.

## 0.6.2

### Bug fix

- Fix infinite rebuild loop with interface files being always re-added.

## 0.6.1

### New Features

- Depend on latest `@rescript/core`.

## 0.6.0

### New Features

- _breaking_ `ResGraphSchemaAssets.res` is no more, and you can remove it. Instead, all interfaces will have their own generated files emitted like `Interface_<interfaceName>.res`. The contents are also slightly restructured to help with type inference and reduce likelihood of circular dependencies. https://github.com/zth/resgraph/pull/27

### Bug fix

- Ensure project folders exist before we try to build.

## 0.5.0

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
