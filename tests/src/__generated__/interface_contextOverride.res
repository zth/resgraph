/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("contextOverride") type t = ExplicitContextOverrideResult(AppExplicitInterfaceImplements.explicitContextOverrideResult)
}

module ImplementedBy = {
  type t = ExplicitContextOverrideResult

  let decode = (str: string) => switch str {
    | "ExplicitContextOverrideResult" => Some(ExplicitContextOverrideResult)
    | _ => None
  }

  external toString: t => string = "%identity"
}
