@gql.type
module User = {
  type t = {name: string, @gql.field age: int}

  @gql.field
  let id = user => {
    ("User" ++ user.name)->ResGraph.id
  }

  @gql.field
  let name = (user, ~includeFullName) => {
    let includeFullName = includeFullName->Belt.Option.getWithDefault(false)

    if includeFullName {
      user.name
    } else {
      user.name->Js.String2.slice(~from=0, ~to_=3)
    }
  }
}

@gql.type
module Query = {
  type t = {}

  @gql.field
  let me = _ => {
    Some({User.name: "Hello", age: 35})
  }
}

// ^gen
