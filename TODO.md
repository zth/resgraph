- [x] Input objects
- [x] Mutations
- [x] Subscriptions (definition, but behavior is broken)
- [x] Interfaces
- [x] Rethink root value for query/subscriptions/mutations
- [x] Validate that enums are only variant cases without payloads
- [x] Have some way to propagate diagnostics somewhere
- [x] Tag loc properly on all items
- [x] Fix all names (so they're uppercased)
- [x] Inject ctx
- [x] Clear up logic that handles when something is persisted. Just going from the types artifacts is probably not enough, we might need to ensure that we always find the correct module path from the right file.
- [x] Nullability in input object fields
- [x] Nullability in arguments
- [x] Runtime conversion for arguments to match ReScript representation
- [x] Runtime conversion for input objects to match ReScript representation
- [ ] How to import and use existing things from the ecosystem?
- [x] Think about and fix compilation
- [x] Traverse structure cache/circuit breaker
- [x] Interface as root type -> add resolver/field to all implementors
- [ ] Directives
- [x] Custom scalars
- [x] Custom scalars with custom serializer/parser
- [x] ~~Figure out a good way to bridge graphql-js validations (print dummy schema without resolvers and run validation on that?)~
- [x] Sort alphabetically in schema output to simplify conflicts/make regeneration stable
- [x] Write empty schema def to prevent type errors on errors from ResGraph
- [x] Fix so that the same diagnostic is never added more than once
- [x] Implement the most important schema validations from `graphql-js` directly in ResGraph.
- [ ] Arguments in interface fields? Fields in interfaces to force implementation of for each type?
- [x] Interfaces picked up that nobody implements should not be printed at all
- [ ] Optimize prop resolution
- [ ] Valdiate duplicate names on adding things
- [x] Use `resgraph.json` as base everywhere, and allow CLI to override as needed
- [x] Allow dumping schema to SDL
- [ ] Init command
- [ ] CI check command
- [x] Allow abstract custom scalars to not have serialize/parse when we can inspect the implementation to be a serializable GraphQL type
- [x] Allow customizing enum case value via `@as`.
- [ ] Config file JSON schema.
- [x] Query/mutation as abstract types.
- [ ] Figure out how to do subscriptions properly. Async iterators? What else exists?
- [ ] Should custom scalar parseValue/serialize have an explicit annotation?
- [ ] Peer deps for `graphql` and `graphql-yoga`

## Ideas

- [x] Inline records in unions to synthetsize types?
- [ ] Shorthand resolver? Reusable resolver? Case is "I have a `userId` and just want to use an existing resolver fn to use it"

## Project integration

- Example with `graphql-yoga`, compression, and so on

## Future

- [ ] One-off generated types for polyvariants. Input objects, input enums, etc
- [ ] Default values for arguments. Would need to parse the AST

## For refactor

- Synthetic types, type creators etc - clean up
- Make id handling more sane

## Documentation

- Ensure snippets are covered
