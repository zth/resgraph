/* @generated */

@@warning("-27-34-37")

@gql.interfaceResolver("hasName")
type hasName_resolver = Group(Schema.group) | Pet(Schema.pet) | User(User.user)

type hasName_implementedBy = Group | Pet | User

let decodeImplementedByInterface_hasName = (str: string) =>
  switch str {
  | "Group" => Some(Group)
  | "Pet" => Some(Pet)
  | "User" => Some(User)
  | _ => None
  }

external hasName_typenameToString: hasName_implementedBy => string = "%identity"

@gql.interfaceResolver("node") type node_resolver = Group(Schema.group) | User(User.user)

type node_implementedBy = Group | User

let decodeImplementedByInterface_node = (str: string) =>
  switch str {
  | "Group" => Some(Group)
  | "User" => Some(User)
  | _ => None
  }

external node_typenameToString: node_implementedBy => string = "%identity"

type node_typeMap<'a> = {
  @as("Group") group: 'a,
  @as("User") user: 'a,
}

module NodeInterfaceTypeMap: {
  type t<'value>
  let make: (node_typeMap<'value>, ~valueToString: 'value => string) => t<'value>

  /** Takes a (stringified) value and returns what type it represents, if any. */
  let getTypeByStringifiedValue: (t<'value>, string) => option<node_implementedBy>

  /** Takes a type and returns what value it represents, as string. */
  let getStringifiedValueByType: (t<'value>, node_implementedBy) => string
} = {
  external unsafe_toDict: node_typeMap<'value> => Js.Dict.t<'value> = "%identity"
  external unsafe_toType: string => node_implementedBy = "%identity"
  type t<'value> = {
    typeToValue: Js.Dict.t<'value>,
    valueToTypeAsString: Js.Dict.t<string>,
    valueToString: 'value => string,
  }
  let make = (typeMap, ~valueToString) => {
    typeToValue: typeMap->unsafe_toDict,
    valueToTypeAsString: typeMap
    ->unsafe_toDict
    ->Js.Dict.entries
    ->Array.map(((key, value)) => (valueToString(value), key))
    ->Js.Dict.fromArray,
    valueToString,
  }

  let getStringifiedValueByType = (t, typ) =>
    t.typeToValue->Dict.get(typ->node_typenameToString)->Option.getExn->t.valueToString
  let getTypeByStringifiedValue = (t, str) =>
    t.valueToTypeAsString->Dict.get(str)->Option.map(unsafe_toType)
}
