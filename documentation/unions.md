You define a union by defining a variant with payloads of types tagged with `@gql.type`, and annotate that variant with `@gql.union`:

```rescript
@gql.type
type user = {
  @gql.field name: string,
  @gql.field age: int
}

@gql.type
type group = {
  @gql.field displayName: string,
}

@gql.union
type entity = User(user) | Group(group)
```

```graphql
type User {
  name: String!
  age: Int!
}

type Group {
  displayName: String!
}

union Entity = User | Group
```

Each variant case can be called whatever you want it to (although it's good practice to follow the name of the GraphQL type it holds), but rememeber that the payload of each union variant case must be exactly 1 type that has a `@gql.type` annotation. But don't worry, ResGraph will complain if you try anything else.

You can add comments on the union type definition itself, as well as each variant case, and they'll end up in the schema.

## Using unions in the schema

Unions can be used as the type for fields on GraphQL objects or interfaces. A simple example:

```rescript
@gql.type
type user = {
  @gql.field name: string,
  @gql.field age: int
}

@gql.type
type group = {
  @gql.field displayName: string,
}

@gql.union
type entity = User(user) | Group(group)

@gql.field
let entity = async (_: query, ~entityId, ~ctx: ResGraphContext.context): option<entity> => {
  switch decodeEntityId(entityId) {
  | Some(#User, id) =>
    switch await ctx.dataLoaders.userById(~userId=id) {
    | None => None
    | Some(user) => Some(User(user))
    }
  | Some(#Group, id) =>
    switch await ctx.dataLoaders.groupById(~groupId=id) {
    | None => None
    | Some(group) => Some(Group(group))
    }
  | _ => None
  }
}

```

```graphql
type User {
  name: String!
  age: Int!
}

type Group {
  displayName: String!
}

union Entity = User | Group

type Query {
  entity(entityId: String!): Entity
}
```

Now that we've covered unions, we can move on to [interfaces](interfaces).
