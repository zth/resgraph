/** Custom scalar with specifiedByUrl coverage. */
@specifiedBy("https://example.com/specifiedBy/uuid")
@gql.annotate({name: "tag", args: {name: "scalar"}})
@gql.scalar
type uuid = string
// ^hov

@gql.type
type scalarHolder = {
  @gql.field id: uuid,
}

@gql.field
let getScalarHolder = (_: Query.query): scalarHolder => {id: "abc"}
// ^hov
