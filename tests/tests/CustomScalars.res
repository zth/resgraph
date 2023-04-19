module TimestampZ = {
  @gql.scalar
  type t = string

  open ResGraph.GraphQLLiteralValue

  let parseValue = v =>
    switch v {
    | String(str) => Some(str)
    | _ => None
    }

  let serialize = str => String(str)
}

// Adding an Inner module here just to make sure things work as intended with
// nested modules.
module Inner = {
  module TimestampHidden: {
    /** A timestamp, but with the implementation hidden in the server. */
    @gql.scalar
    type t

    let parseValue: ResGraph.GraphQLLiteralValue.t => option<t>
    let serialize: t => ResGraph.GraphQLLiteralValue.t
  } = {
    type t = string

    open ResGraph.GraphQLLiteralValue

    let parseValue = v =>
      switch v {
      | String(str) => Some(str)
      | _ => None
      }

    let serialize = str => String(str)
  }
}
