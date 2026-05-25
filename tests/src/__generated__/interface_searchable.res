/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("searchable")
  type t = ExplicitSearchResult(AppExplicitInterfaceImplements.explicitSearchResult)
}

module ImplementedBy = {
  type t = ExplicitSearchResult

  let decode = (str: string) =>
    switch str {
    | "ExplicitSearchResult" => Some(ExplicitSearchResult)
    | _ => None
    }

  external toString: t => string = "%identity"
}
