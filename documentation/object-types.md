Define an object type using the annotation `@gql.type`.

```rescript
@gql.type
type user = {
  name: string,
  age: int
}
```

- Must be a record type
- By default, no fields are exposed

### Exposing fields without a dedicated resolver

You can expose a field as-is........
