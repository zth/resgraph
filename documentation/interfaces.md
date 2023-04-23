## Defining an interface

- Must be records

## Implementing an interface

You implement an interface on a type by spreading that interface on that type definition. Example:

```rescript
/** An entity with a name. */
@gql.interface
type hasName = {
  /** The name of the thing. */
  @gql.field name: string
}

/** A user in the system. */
@gql.type
type user = {
  ...hasName,
  @gql.field age: int
}
```

```graphql
"""
An entity with a name.
"""
interface HasName {
  """
  The name of the thing.
  """
  name: String!
}

type User implements HasName {
  name: String!
  age: Int!
}
```

## Exposing fields from the interface

### Interface resolvers

- Are added to each type implementing that interface automatically
- Can override by defining a dedicated version of that field

## Interfaces as return type for field

- How to implement them
- Example
