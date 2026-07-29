@gql.type
type query

@gql.field
let legacy = (_: query): string => "legacy"

@gql.field
let newField = (_: query): string => "new"
