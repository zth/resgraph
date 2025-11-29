/* @generated */

@@warning("-27-34-37")

module Resolver = {
  @gql.interfaceResolver("node") type t = Thing(Thing.thing)
}

module ImplementedBy = {
  type t = Thing

  let decode = (str: string) =>
    switch str {
    | "Thing" => Some(Thing)
    | _ => None
    }

  external toString: t => string = "%identity"
}

type typeMap<'a> = {@as("Thing") thing: 'a}

module TypeMap: {
  type t<'value>
  let make: (typeMap<'value>, ~valueToString: 'value => string) => t<'value>

  /** Takes a (stringified) value and returns what type it represents, if any. */
  let getTypeByStringifiedValue: (t<'value>, string) => option<ImplementedBy.t>

  /** Takes a type and returns what value it represents, as string. */
  let getStringifiedValueByType: (t<'value>, ImplementedBy.t) => string
} = {
  external unsafe_toDict: typeMap<'value> => dict<'value> = "%identity"
  external unsafe_toType: string => ImplementedBy.t = "%identity"
  type t<'value> = {
    typeToValue: dict<'value>,
    valueToTypeAsString: dict<string>,
    valueToString: 'value => string,
  }
  let make = (typeMap, ~valueToString) => {
    typeToValue: typeMap->unsafe_toDict,
    valueToTypeAsString: typeMap
    ->unsafe_toDict
    ->Dict.toArray
    ->Array.map(((key, value)) => (valueToString(value), key))
    ->Dict.fromArray,
    valueToString,
  }

  let getStringifiedValueByType = (t, typ) =>
    t.typeToValue
    ->Dict.get(typ->ImplementedBy.toString)
    ->Option.getOrThrow
    ->t.valueToString
  let getTypeByStringifiedValue = (t, str) =>
    t.valueToTypeAsString->Dict.get(str)->Option.map(unsafe_toType)
}
