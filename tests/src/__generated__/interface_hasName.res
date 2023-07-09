/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("hasName") type t = Group(Schema.group) | Pet(Schema.pet) | User(User.user)
}

module ImplementedBy = {
  type t = Group | Pet | User

  let decode = (str: string) =>
    switch str {
    | "Group" => Some(Group)
    | "Pet" => Some(Pet)
    | "User" => Some(User)
    | _ => None
    }

  external toString: t => string = "%identity"
}
