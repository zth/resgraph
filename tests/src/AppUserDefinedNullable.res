module MyDomain = {
  module Nullable = {
    @gql.type
    type t = {
      @gql.field
      value: string,
    }
  }
}

@gql.field
let userDefinedNullable = (_: Query.query): MyDomain.Nullable.t => {
  value: "user-defined nullable module",
}
