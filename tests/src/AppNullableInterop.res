@gql.type
type nullableInterop = {
  @gql.field
  nullCount: Null.t<int>,
  @gql.field
  nullableName: Nullable.t<string>,
}

@gql.field
let nullableInterop = (
  _: Query.query,
  ~nullCount: Null.t<int>,
  ~nullableName: Nullable.t<string>,
) => {
  nullCount,
  nullableName,
}
