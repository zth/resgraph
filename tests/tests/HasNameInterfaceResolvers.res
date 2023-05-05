@gql.field
let hasName = (_: Query.query, ~id: ResGraph.id): option<ResGraphSchemaAssets.hasName_resolver> => {
  ignore(id)
  Some(User({id: "123", name: "Test", lastAge: Some(35), age: 35}))
}

@gql.field
let abbreviatedName = (
  // ^hov
  name: HasNameInterface.hasName,
  ~typeName: ResGraphSchemaAssets.hasName_implementedBy,
) => {
  Some(typeName->ResGraphSchemaAssets.hasName_typenameToString ++ ":" ++ name.name)
}
