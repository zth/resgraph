Input objects are defined by using a record annotated with `@gql.inputObject`:

```rescript
/** Config for searching for a user. */
@gql.inputObject
type userSearchConfig = {
  groupId?: ResGraph.id,
  name?: string,
}

@gql.field
let searchForUser = (_: query, ~input: userConfig, ~ctx: ResGraphContext.context): option<user> => {
  ctx.dataLoaders.searchForUser.load(~name=input.name, ~groupId=input.groupId)
}
```

```graphql
input UserSearchConfig {
  groupId: ID
  name: String
}

type Query {
  searchForUser(input: UserSearchConfig!): User
}
```

One big difference compared to [object type fields](object-types#fields) is that in input objects, all fields are automatically exposed, because the contrary wouldn't make sense since this is an input from the client and not something you're supposed to construct yourself.

### Comments

You can add comments to the type definition itself, and to all record fields. These will then be exposed in your schema.

### Handling `null`

Just like in [arguments of object type fields](object-types#handling-null-in-arguments), you can choose to explicitly handle `null` values by annotating fields in the input object to be `Js.Nullable.t`.

### Recursive input objects

Input objects are allowed to be (mutually) recursive, if they're not recursive in a non-nullable way, as that would create an endless loop.

In ReScript, you'd handle that scenario like you'd normally handle mutually recursive types:

```rescript
/** Some user config. */
@gql.inputObject
type rec userConfig = {
  name?: string,
  additionalConfig?: userConfig,
  lastGroupConfig?: groupConfig,
}
@gql.inputObject /** Some group config. */
and groupConfig = {
  groupName: string,
  additionalUserConfig?: userConfig,
}
```
