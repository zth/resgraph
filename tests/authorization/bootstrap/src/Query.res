@gql.type
type query

@gql.type
type legacyRecord = {
  @gql.field
  id: string,
}

@gql.field
let legacy = (_: query): legacyRecord => {id: "legacy"}
