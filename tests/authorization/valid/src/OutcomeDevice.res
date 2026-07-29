@gql.implements("OutcomeNamed") @gql.type
type outcomeDevice = {
  @gql.field
  id: string,
}

@gql.implements("OutcomeNamed") @gql.type
type inheritedOutcomeDevice = {
  @gql.field
  id: string,
}

@gql.public({reason: "Concrete override has its own public disposition"}) @gql.field
let computed = (_: outcomeDevice): string => "concrete override"
