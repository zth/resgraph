@gql.type
type mutation

@gql.authorize.byAncestor({reason: "Mutation payload fields are governed by the mutation policy"})
@gql.type
type updatePayload = {
  @gql.field
  value: string,
}

@gql.authorize(Security.canMutate) @gql.field
let update = (_: mutation, ~value: string): updatePayload => {value: value}

@gql.public({reason: "Explicitly public mutation used by uptime checks"}) @gql.field
let ping = (_: mutation): bool => true
