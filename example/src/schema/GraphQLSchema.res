@gql.type
type query = {}

@gql.type
type user = {@gql.field name: string, @gql.field age: int}

@gql.field
let me = (_: query) => {
  {name: "Mr test", age: 35}
}
