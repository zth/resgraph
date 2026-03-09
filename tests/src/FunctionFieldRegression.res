@gql.type
type functionFieldRegression = {
  @gql.field id: string,
}

let make = (): functionFieldRegression => {
  id: "function-field-regression",
}

@gql.field
let functionFieldRegression = (_: Query.query) => make()

@gql.field
let computedLabel = (_: functionFieldRegression) => "computed label"
