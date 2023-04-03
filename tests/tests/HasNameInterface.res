/** An entity with a name. */
@gql.interface
type hasName = {@gql.field name: string}

@gql.field
let hasName = (_: Query.query, ~id: ResGraph.id): option<ResGraphSchemaAssets.hasName_resolver> => {
  ignore(id)
  Some(User({name: "Test", lastAge: Some(35), age: 35}))
}
