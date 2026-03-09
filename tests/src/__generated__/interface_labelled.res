/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("labelled")
  type t =
    LabelledAlpha(AppLabelledTypes.labelledAlpha) | LabelledBeta(AppLabelledTypes.labelledBeta)
}

module ImplementedBy = {
  type t = LabelledAlpha | LabelledBeta

  let decode = (str: string) =>
    switch str {
    | "LabelledAlpha" => Some(LabelledAlpha)
    | "LabelledBeta" => Some(LabelledBeta)
    | _ => None
    }

  external toString: t => string = "%identity"
}
