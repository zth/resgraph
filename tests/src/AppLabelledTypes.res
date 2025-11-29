@gql.interface
type labelled = {name: string}

@gql.type
type labelledAlpha = {
  ...labelled,
  @gql.field extra: string,
}

@gql.type
type labelledBeta = {
  ...labelled,
  @gql.field count: int,
}

@gql.field
let getLabelled = (_: Query.query): labelledAlpha => {name: "Alpha", extra: "hey"}
