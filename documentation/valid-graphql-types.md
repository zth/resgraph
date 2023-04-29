Given that fields and arguments will be exposed to the outside world through your schema, they can only be of types that can be transformed to valid GraphQL types.

Valid GraphQL types are:

- All primitives that map to a corresponding GraphQL primitive: `string`, `array`, `option`, `bool`, `int`, `float`.
- Nullable "builtin" types: `Nullable.t`, `Null.t`, etc.
- Any types annotated with valid `@gql` annotations.

ResGraph will complain loudly if you try and use other types, and typically tell you how to fix your issue.

### Special case: `ID`

If you find yourself wanting to expose a field or argument using the GraphQL built in type `ID`, you'll need to annotate that thing with `ResGraph.id`. `ResGraph.id` is an opaque type that easily can be converted to and from `string`, which is its underlying type. An example:

```rescript
@gql.type
type user = {
  id: string
}

@gql.field
let id = (user: user) => {
  id->ResGraph.id
}

@gql.field
let userById = (_: query, ~id: ResGraph.id, ~ctx: ResGraphContext.context) => {
  ctx.dataLoaders.userById.load(~userId=id->ResGraph.idToString)
}
```

```graphql
type User {
  id: ID!
}

type Query {
  userById(id: ID!): User
}
```

`ID` is a special GraphQL type, and you're encouraged to use it only at the GraphQL boundaries, rather than for example annotating the `id` field on `user` above with `ResGraph.id` directly.

### Custom scalars

If you end up in a situation where you need a field or argument to be of a type that's not a valid GraphQL type by default, you can leverage custom scalars to define a type that can be whatever you need it to be internally, and is exposed through your schema and to your client as a custom scalar. Read more about [custom scalars](#custom-scalars) here.
