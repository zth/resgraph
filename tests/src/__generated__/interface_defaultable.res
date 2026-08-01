/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("defaultable")
  type t = ExplicitDefaultable(AppExplicitInterfaceImplements.explicitDefaultable)
}

module ImplementedBy = {
  type t = ExplicitDefaultable

  let decode = (str: string) => switch str {
    | "ExplicitDefaultable" => Some(ExplicitDefaultable)
    | _ => None
  }

  external toString: t => string = "%identity"
}
