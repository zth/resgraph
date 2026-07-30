@gql.interface
type outcomeNamed = {
  @gql.public({reason: "Identifiers are public across this interface"}) @gql.field
  id: string,
}

@gql.field
let computed = (value: outcomeNamed): ResGraph.Authorization.outcome<
  string,
  string,
> => ResGraph.Authorization.Allowed(value.id)
