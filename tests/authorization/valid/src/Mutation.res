@gql.type
type mutation

@gql.authorize(Security.canMutate) @gql.field
let update = (_: mutation, ~value: string): string => value

@gql.public({reason: "Explicitly public mutation used by uptime checks"}) @gql.field
let ping = (_: mutation): bool => true
