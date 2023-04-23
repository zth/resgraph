@gql.type
type query = {}

@gql.type
type user = {
  ...HasNameInterface.hasName,
  @gql.field age: int,
}

@gql.field
let me = (_: query, ~onlyIfAvailable) => {
  if onlyIfAvailable {
    Some({name: "Test User", age: 35})
  } else {
    None
  }
}
