@gql.type
type query = {}

@gql.type
type user = {
  ...HasNameInterface.hasName,
  @gql.field age: int,
}

@gql.field
let me = (_: query) => {
  Some({name: "Test User", age: 35})
}
