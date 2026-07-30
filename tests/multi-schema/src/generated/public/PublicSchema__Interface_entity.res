/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("entity")
  type t = SharedItem(Shared.sharedItem)
}

module ImplementedBy = {
  type t = SharedItem

  let decode = (str: string) => switch str {
    | "SharedItem" => Some(SharedItem)
    | _ => None
  }

  external toString: t => string = "%identity"
}
