open GenerateSchemaTypes

let locationToString = function
  | LocationQuery -> "QUERY"
  | LocationMutation -> "MUTATION"
  | LocationSubscription -> "SUBSCRIPTION"
  | LocationField -> "FIELD"
  | LocationFragmentDefinition -> "FRAGMENT_DEFINITION"
  | LocationFragmentSpread -> "FRAGMENT_SPREAD"
  | LocationInlineFragment -> "INLINE_FRAGMENT"
  | LocationVariableDefinition -> "VARIABLE_DEFINITION"
  | LocationSchema -> "SCHEMA"
  | LocationScalar -> "SCALAR"
  | LocationObject -> "OBJECT"
  | LocationFieldDefinition -> "FIELD_DEFINITION"
  | LocationArgumentDefinition -> "ARGUMENT_DEFINITION"
  | LocationInterface -> "INTERFACE"
  | LocationUnion -> "UNION"
  | LocationEnum -> "ENUM"
  | LocationEnumValue -> "ENUM_VALUE"
  | LocationInputObject -> "INPUT_OBJECT"
  | LocationInputFieldDefinition -> "INPUT_FIELD_DEFINITION"

let locationOfString = function
  | "QUERY" -> Some LocationQuery
  | "MUTATION" -> Some LocationMutation
  | "SUBSCRIPTION" -> Some LocationSubscription
  | "FIELD" -> Some LocationField
  | "FRAGMENT_DEFINITION" -> Some LocationFragmentDefinition
  | "FRAGMENT_SPREAD" -> Some LocationFragmentSpread
  | "INLINE_FRAGMENT" -> Some LocationInlineFragment
  | "VARIABLE_DEFINITION" -> Some LocationVariableDefinition
  | "SCHEMA" -> Some LocationSchema
  | "SCALAR" -> Some LocationScalar
  | "OBJECT" -> Some LocationObject
  | "FIELD_DEFINITION" -> Some LocationFieldDefinition
  | "ARGUMENT_DEFINITION" -> Some LocationArgumentDefinition
  | "INTERFACE" -> Some LocationInterface
  | "UNION" -> Some LocationUnion
  | "ENUM" -> Some LocationEnum
  | "ENUM_VALUE" -> Some LocationEnumValue
  | "INPUT_OBJECT" -> Some LocationInputObject
  | "INPUT_FIELD_DEFINITION" -> Some LocationInputFieldDefinition
  | _ -> None

let locationForTarget = function
  | DirectiveSchema -> LocationSchema
  | DirectiveScalar _ -> LocationScalar
  | DirectiveObject _ -> LocationObject
  | DirectiveFieldDefinition _ -> LocationFieldDefinition
  | DirectiveArgumentDefinition _ -> LocationArgumentDefinition
  | DirectiveDirectiveArgumentDefinition _ -> LocationArgumentDefinition
  | DirectiveInterface _ -> LocationInterface
  | DirectiveUnion _ -> LocationUnion
  | DirectiveEnum _ -> LocationEnum
  | DirectiveEnumValue _ -> LocationEnumValue
  | DirectiveInputObject _ -> LocationInputObject
  | DirectiveInputFieldDefinition _ -> LocationInputFieldDefinition

let targetToString = function
  | DirectiveSchema -> "schema"
  | DirectiveScalar name
  | DirectiveObject name
  | DirectiveInterface name
  | DirectiveUnion name
  | DirectiveEnum name
  | DirectiveInputObject name ->
    name
  | DirectiveFieldDefinition {parentTypeName; fieldName} ->
    parentTypeName ^ "." ^ fieldName
  | DirectiveArgumentDefinition {parentTypeName; fieldName; argumentName} ->
    Printf.sprintf "%s.%s(%s:)" parentTypeName fieldName argumentName
  | DirectiveDirectiveArgumentDefinition {directiveName; argumentName} ->
    Printf.sprintf "@%s(%s:)" directiveName argumentName
  | DirectiveEnumValue {enumName; valueName} -> enumName ^ "." ^ valueName
  | DirectiveInputFieldDefinition {inputObjectName; fieldName} ->
    inputObjectName ^ "." ^ fieldName
