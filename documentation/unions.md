You define a union by defining a variant with payloads of types tagged with `@gql.type`, and tag that variant with `@gql.union`:

```rescript
@gql.type
type user = {
  name: string,
  age: int
}
@gql.union
type userStatus = User(user) | Group(group)
```

```graphql
enum UserStatus {
  Online
  Offline
}
```
