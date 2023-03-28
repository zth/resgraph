- [ ] Input objects
- [ ] Interfaces
- [ ] Validate that enums are only variant cases without payloads
- [ ] Have some way to propagate diagnostics somewhere
- [ ] Validate uniqueness of names? (just let graphql-js do that?)
- [ ] Tag loc properly on all items
- [x] Fix all names (so they're uppercased)
- [ ] Allow customizing typename
- [x] Inject ctx
- [ ] Clear up logic that handles when something is persisted. Just going from the types artifacts is probably not enough, we might need to ensure that we always find the correct module path from the right file.

## Future

- [ ] Relay helpers
- [ ] One-off generated types for polyvariants. Input objects, input enums, etc

## Validations

- Object/input object has no fields (annotate at least one with @gql.field, or create a resolver)

## Code actions

- Expose field as GraphQL field
- Generate resolver for type
