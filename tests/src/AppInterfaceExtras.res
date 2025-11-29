open AppLabelledTypes
open Interface_labelled

@gql.field
let typenameEcho = (
  item: labelled,
  ~typeName: Interface_labelled.ImplementedBy.t,
) => item.name ++ ":" ++ ImplementedBy.toString(typeName)
