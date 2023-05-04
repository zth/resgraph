module TimestampZ = {
  @gql.scalar
  type t = string
  //   ^hov

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
    type t = Date.t

    open ResGraph.GraphQLLiteralValue

    let parseValue = v =>
      switch v {
      | String(str) => Some(Date.fromString(str))
      | Number(timestamp) => Some(Date.fromTime(timestamp))
      | _ => None
      }

    let serialize = d => d->Date.toJSON->Option.getExn->String
  }
}

// This is a custom scalar with a hidden implementation but where the
// implementation is serializable to GraphQL under the hood.
module TimestampHiddenSerializable: {
  /** A timestamp, but with the implementation hidden in the server. Under the
    hood, it's serializable. */
  @gql.scalar
  type t
  let make: string => t
} = {
  type t = string
  let make = str => str
}
