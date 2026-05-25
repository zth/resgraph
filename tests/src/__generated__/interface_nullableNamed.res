/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("nullableNamed")
  type t = ExplicitCompany(AppExplicitInterfaceImplements.explicitCompany)
}

module ImplementedBy = {
  type t = ExplicitCompany

  let decode = (str: string) =>
    switch str {
    | "ExplicitCompany" => Some(ExplicitCompany)
    | _ => None
    }

  external toString: t => string = "%identity"
}
