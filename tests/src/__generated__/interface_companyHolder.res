/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("companyHolder")
  type t = ExplicitCompanyHolder(AppExplicitInterfaceImplements.explicitCompanyHolder)
}

module ImplementedBy = {
  type t = ExplicitCompanyHolder

  let decode = (str: string) => switch str {
    | "ExplicitCompanyHolder" => Some(ExplicitCompanyHolder)
    | _ => None
  }

  external toString: t => string = "%identity"
}
