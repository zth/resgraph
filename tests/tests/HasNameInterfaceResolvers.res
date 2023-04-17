@gql.field
let hasName = (_: Query.query, ~id: ResGraph.id): option<ResGraphSchemaAssets.hasName_resolver> => {
  ignore(id)
  Some(User({id: "123"->ResGraph.id, name: "Test", lastAge: Some(35), age: 35}))
}

@gql.field
let abbreviatedName = (name: HasNameInterface.hasName) => {
  Some(name.name)
}
