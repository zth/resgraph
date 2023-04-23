- [x] Input objects
- [x] Mutations
- [x] Subscriptions (definition, but behavior is broken)
- [x] Interfaces
- [ ] Rethink root value for query/subscriptions/mutations
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
- [ ] Figure out how to do subscriptions properly. Async iterators? What else exists?
- [ ] How to import and use existing things from the ecosystem?
- [x] Think about and fix compilation
- [x] Traverse structure cache/circuit breaker
- [x] Interface as root type -> add resolver/field to all implementors
- [ ] Directives
- [x] Custom scalars
- [x] Custom scalars with custom serializer/parser
- [ ] Should custom scalar parseValue/serialize have an explicit annotation?
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

## Bugs

- [x] Custom scalar isn't inferred in return position (probably just walked on by)
- [x] Custom scalar in module is shaky

## Project integration

- Example with `graphql-yoga`, compression, and so on
- Get and construct app context from request

## Future

- [ ] Relay helpers
- [ ] One-off generated types for polyvariants. Input objects, input enums, etc
- [ ] Custom scalars with automatic conversion (is this already natively supported perhaps?)
- [ ] Default values for arguments. Would need to parse the AST which I'm trying to avoid (can maybe do it lazily only if there are resolvers?)

## Relay

- [ ] Opt in to nodeInterface
- [ ] Node interface ID helpers
- [ ] Basic connection type

## Validations

- [x] Object has no fields (annotate at least one with @gql.field, or create a resolver)
- [x] Input object has invalid field types

## Code actions

- Expose field as GraphQL field
- Generate resolver for type

## Questions to answer

- Should the node interface come built in...? Or just show how to define it easily?
- How to reuse resolvers for several types? Just link/expose like you want, the tool only cares about the types
