/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("contextual") type t = ExplicitContextResult(AppExplicitInterfaceImplements.explicitContextResult)
}

module ImplementedBy = {
  type t = ExplicitContextResult

  let decode = (str: string) => switch str {
    | "ExplicitContextResult" => Some(ExplicitContextResult)
    | _ => None
  }

  external toString: t => string = "%identity"
}
