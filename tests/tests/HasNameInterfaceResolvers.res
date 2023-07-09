open Interface_hasName

@gql.field
let hasName = (_: Query.query, ~id: ResGraph.id): option<Resolver.t> => {
  ignore(id)
  Some(User({id: "123", name: "Test", lastAge: Some(35), age: 35}))
}

@gql.field
let abbreviatedName = (
  // ^hov
  name: HasNameInterface.hasName,
  ~typeName: ImplementedBy.t,
) => {
  Some(typeName->ImplementedBy.toString ++ ":" ++ name.name)
}
