@gql.type
type query

@gql.field
let invalid = (_: query): Date.t => Date.make()
