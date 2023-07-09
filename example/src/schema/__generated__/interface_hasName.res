/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("hasName") type t = User(GraphQLSchema.user)
}

module ImplementedBy = {
  type t = User

  let decode = (str: string) =>
    switch str {
    | "User" => Some(User)
    | _ => None
    }

  external toString: t => string = "%identity"
}
