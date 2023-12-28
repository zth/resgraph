---
sidebar_position: 8
---

# Input Unions

Even though they're not officially in the spec yet, ResGraph has first class support for [input unions via the `@oneOf` server directive proposal](https://github.com/graphql/graphql-spec/pull/825).

## Prerequisites

You'll need to add the `@oneOf` validation package and configure it in your server to be able to use input unions. It looks like this:

```bash
# First, install the package that has the validation rule. This is using graphql-yoga@3.
npm i @envelop/extended-validation@2
```

Next, add the plugin to your GraphQL Yoga server:

```rescript
open GraphQLYoga

let yoga = createYoga({
  schema: ResGraphSchema.schema,
  plugins: [
    Envelope.Plugin.ExtendedValidation.use({
      rules: [Envelope.Plugin.ExtendedValidation.Rule.oneOfInputObjectsRule],
    }),
  ],
```

Now you should be all set!

> You can [read more here](https://the-guild.dev/graphql/envelop/docs/guides/using-graphql-features-from-the-future) on how this plugin works with GraphQL Yoga.

## Using Input Unions

Input unions are unions that can be used as inputs for fields and mutations. Input unions are regular variants in ResGraph, where the payload can be:

- Any valid GraphQL type that can be used in an input position
- An inline record

Using an inline record will produce a new input object type for only this inline record.

Input unions are defined by using a variant annotated with `@gql.inputUnion`. Full example:

```rescript
/** Searching for a user by group. */
@gql.inputObject
type userSearchByGroupConfig = {
  groupId: ResGraph.id,
  userMemberToken?: string,
}

/** Config for searching for a user. */
@gql.inputUnion
type userSearchConfig = ByGroup(userSearchByGroupConfig) | ByName(string) | ByUserToken({userToken: ResGraph.id})

@gql.field
let searchForUser = (_: query, ~input: userSearchConfig, ~ctx: ResGraphContext.context): option<user> => {
  switch input {
  | ByGroup({groupId, userMemberToken}) => ctx.dataLoaders.searchForUserByGroup.load(~userMemberToken, ~groupId)
  | ByName({groupId, userMemberToken}) => ctx.dataLoaders.searchForUserByName.load(name)
  | ByUserToken({userToken}) => ctx.dataLoaders.searchForUserByToken.load(userToken)
  }

}
```

```graphql
"""
Searching for a user by group.
"""
input UserSearchByGroupConfig {
  groupId: ID!
  userMemberToken: String
}

input UserSearchConfigByUserToken {
  userToken: ID!
}

"""
Config for searching for a user.
"""
input UserSearchConfig @oneOf {
  byGroup: UserSearchByGroupConfig
  byName: String
  byUserToken: UserSearchConfigByUserToken
}

type Query {
  searchForUser(input: UserSearchConfig!): User
}
```

As with regular input objects, all fields are automatically exposed.

### Comments

You can add comments to the type definition itself, and to all record fields. These will then be exposed in your schema.

### Handling `null`

Just like in [arguments of object type fields](object-types#handling-null-in-arguments), you can choose to explicitly handle `null` values by annotating any field or member in the input union to be `Js.Nullable.t`.

### Recursive input unions

As with input objects, input unions are allowed to be (mutually) recursive, if they're not recursive in a non-nullable way, as that would create an endless loop.

Read more [in the input object docs](input-objects).
