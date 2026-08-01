---
sidebar_position: 8
---

# Input Unions

ResGraph models input unions as standard [OneOf Input Objects](https://spec.graphql.org/September2025/#sec-OneOf-Input-Objects).
They use graphql-js's native validation, input coercion, SDL, and introspection
support. No validation plugin is required.

Install `graphql@^16.11` or `graphql@^17` alongside ResGraph. GraphQL 17
requires Node 22 or newer; GraphQL 16 remains supported for Node 20 projects.

## Using Input Unions

Input unions are unions that can be used as inputs for fields and mutations. Input unions are regular variants in ResGraph, where the payload can be:

- Any valid GraphQL type that can be used in an input position
- An inline record

Using an inline record will produce a new input object type for only this inline record.

Inline record fields follow the same naming rules as regular [input object fields](input-objects#customizing-field-names-with-as). If the GraphQL input field should be a reserved ReScript name such as `constraint`, use `@as` on the inline record field:

```rescript
@gql.inputUnion
type ruleInput =
  | Rule({
      @as("constraint")
      constraint_: string,
    })
```

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
