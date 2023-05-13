type kind =
  | Name
  | Document
  | OperationDefinition
  | VariableDefinition
  | SelectionSet
  | Field
  | Argument
  | ListNullabilityOperator
  | NonNullAssertion
  | ErrorBoundary
  | FragmentSpread
  | InlineFragment
  | FragmentDefinition
  | Variable
  | IntValue
  | FloatValue
  | StringValue
  | BooleanValue
  | NullValue
  | ObjectField
  | Directive
  | NamedType
  | ListType
  | NonNullType
  | SchemaDefinition
  | OperationTypeDefinition
  | ScalarTypeDefinition
  | ObjectTypeDefinition
  | FieldDefinition
  | InputValueDefinition
  | InterfaceTypeDefinition
  | UnionTypeDefinition
  | EnumTypeDefinition
  | EnumValueDefinition
  | InputObjectTypeDefinition
  | DirectiveDefinition
  | SchemaExtension
  | ScalarTypeExtension
  | ObjectTypeExtension
  | InterfaceTypeExtension
  | UnionTypeExtension
  | EnumTypeExtension
  | InputObjectTypeExtension
  | AliasedField
  | Arguments
  | ShortQuery
  | Query
  | Mutation
  | Subscription
  | TypeCondition
  | Invalid
  | Comment
  | SchemaDef
  | ScalarDef
  | ObjectTypeDef
  | ObjectValue
  | ListValue
  | InterfaceDef
  | UnionDef
  | EnumDef
  | EnumValue
  | FieldDef
  | InputDef
  | InputValueDef
  | ArgumentsDef
  | ExtendDef
  | ExtensionDefinition
  | DirectiveDef
  | Implements
  | VariableDefinitions
  | Type

@unboxed type nullable<'t> = Value('t) | @as(null) Null | @as(undefined) Undefined

type rec state = {
  prevState: nullable<state>,
  name: nullable<string>,
  kind: nullable<kind>,
}
type contextToken = {
  start: int,
  end: int,
  string: string,
  state: state,
  style?: string,
}
type position = {
  line: int,
  character: int,
}

@module("graphql-language-service")
external getTokenAtPosition: (~queryText: string, ~position: position) => contextToken =
  "getTokenAtPosition"

type lookupValue = Typename(string) | Field({ownerTypeName: string, fieldName: string})

let findOwner = state => {
  switch state.prevState {
  | Value({kind: Value(kind), name: Value(name)}) => Some(kind, name)
  | _ => None
  }
}

let findLookupValue = token => {
  switch token {
  | {
      state: {
        kind: Value(
          ObjectTypeDef
          | InterfaceDef
          | InputDef
          | UnionDef
          | EnumDef
          | ScalarDef
          | NamedType,
        ),
        name: Value(name),
      },
    } =>
    Some(Typename(name))
  | {state: {kind: Value(FieldDef), name: Value(name)} as state} =>
    switch findOwner(state) {
    | None => None
    | Some(_kind, ownerName) => Some(Field({ownerTypeName: ownerName, fieldName: name}))
    }
  | _ => None
  }
}

let hoverAtPos = (~path, ~pos: LspProtocol.loc) => {
  try {
    let fileContents = Fs.readFileSync(path)->Buffer.toStringWithEncoding(StringEncoding.utf8)

    switch getTokenAtPosition(
      ~queryText=fileContents,
      ~position=(pos :> position),
    )->findLookupValue {
    | None => None
    | Some(Typename(typename)) =>
      switch Utils.callPrivateCli(HoverGraphQL({filePath: path, hoverHint: typename})) {
      | Hover({item}) => Some(item)
      | _ => None
      }
    | Some(Field({ownerTypeName, fieldName})) =>
      switch Utils.callPrivateCli(
        HoverGraphQL({filePath: path, hoverHint: `${ownerTypeName}.${fieldName}`}),
      ) {
      | Hover({item}) => Some(item)
      | _ => None
      }
    }
  } catch {
  | Exn.Error(e) =>
    Console.error(e)
    None
  }
}

let definitionAtPos = (~path, ~pos: LspProtocol.loc) => {
  try {
    let fileContents = Fs.readFileSync(path)->Buffer.toStringWithEncoding(StringEncoding.utf8)

    switch getTokenAtPosition(
      ~queryText=fileContents,
      ~position=(pos :> position),
    )->findLookupValue {
    | None => None
    | Some(Typename(typename)) =>
      switch Utils.callPrivateCli(Definition({filePath: path, definitionHint: typename})) {
      | Definition({item}) => Some(item)
      | _ => None
      }
    | Some(Field({ownerTypeName, fieldName})) =>
      switch Utils.callPrivateCli(
        Definition({filePath: path, definitionHint: `${ownerTypeName}.${fieldName}`}),
      ) {
      | Definition({item}) => Some(item)
      | _ => None
      }
    }
  } catch {
  | Exn.Error(e) =>
    Console.error(e)
    None
  }
}
