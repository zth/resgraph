open GenerateSchemaTypes
open GenerateSchemaUtils

let scalarToString (s : scalar) =
  match s with
  | ID -> "ID"
  | String -> "String"
  | Int -> "Int"
  | Float -> "Float"
  | Boolean -> "Boolean"

let rec graphqlTypeToString ?(nullable = false) (t : graphqlType) =
  let nullableSuffix = if nullable = false then "!" else "" in
  match t with
  | Scalar scalar -> scalarToString scalar ^ nullableSuffix
  | EmptyPayload -> graphqlTypeToString ~nullable:true (Scalar Boolean)
  | Nullable inner | RescriptNullable inner ->
    graphqlTypeToString ~nullable:true inner
  | List inner ->
    Printf.sprintf "[%s]%s" (graphqlTypeToString inner) nullableSuffix
  | GraphQLObjectType {displayName}
  | GraphQLInputObject {displayName}
  | GraphQLEnum {displayName}
  | GraphQLUnion {displayName}
  | GraphQLInputUnion {displayName}
  | GraphQLInterface {displayName}
  | GraphQLScalar {displayName} ->
    Printf.sprintf "%s%s" displayName nullableSuffix
  | InjectInterfaceTypename {interfaceId = intfId} ->
    Printf.sprintf "%s%s" (capitalizeFirstChar intfId) nullableSuffix
  | InjectContext | InjectInfo -> "Unknown"

let indent n =
  let buffer = Buffer.create n in
  for i = 0 to n - 1 do
    Buffer.add_char buffer ' '
  done;
  Buffer.contents buffer

let printImplements interfaces =
  if List.length interfaces > 0 then
    Printf.sprintf " implements %s"
      (interfaces
      |> List.map (fun id -> GenerateSchemaUtils.capitalizeFirstChar id)
      |> String.concat " & ")
  else ""

let printDescription desc indentation =
  match desc with
  | None -> ""
  | Some desc -> Printf.sprintf "\n%s\"\"\"%s\"\"\"\n" (indent indentation) desc

let printDeprecatedDirective deprecationReason =
  match deprecationReason with
  | Some deprecationReason ->
    Printf.sprintf " @deprecated(reason: \"%s\")" deprecationReason
  | None -> ""

let rec constValueToString = function
  | ConstNull -> "null"
  | ConstInt value | ConstFloat value | ConstEnum value -> value
  | ConstString value -> Printf.sprintf "\"%s\"" (Json.escape value)
  | ConstBoolean value -> if value then "true" else "false"
  | ConstList values ->
    Printf.sprintf "[%s]"
      (values |> List.map constValueToString |> String.concat ", ")
  | ConstObject fields ->
    Printf.sprintf "{%s}"
      (fields
      |> List.map (fun (name, value) ->
          Printf.sprintf "%s: %s" name (constValueToString value))
      |> String.concat ", ")

let printDirectiveApplication (application : gqlDirectiveApplication) =
  Printf.sprintf " @%s%s" application.name
    (match application.arguments with
    | [] -> ""
    | arguments ->
      Printf.sprintf "(%s)"
        (arguments
        |> List.map (fun (name, value) ->
            Printf.sprintf "%s: %s" name (constValueToString value))
        |> String.concat ", "))

let printDirectiveApplications schemaState target =
  GenerateSchemaUtils.directivesForTarget schemaState target
  |> List.map printDirectiveApplication
  |> String.concat ""

let printDirectiveDefinition schemaState (definition : gqlDirectiveDefinition) =
  let arguments =
    match definition.arguments with
    | [] -> ""
    | arguments ->
      Printf.sprintf "(\n%s\n)"
        (arguments
        |> List.map (fun (argument : gqlDirectiveArgument) ->
            Printf.sprintf "%s  %s: %s%s%s"
              (printDescription argument.description 2)
              argument.name
              (graphqlTypeToString argument.typ)
              (match argument.defaultValue with
              | None -> ""
              | Some value -> " = " ^ constValueToString value)
              (printDeprecatedDirective argument.deprecationReason
              ^ printDirectiveApplications schemaState
                  (DirectiveDirectiveArgumentDefinition
                     {
                       directiveName = definition.name;
                       argumentName = argument.name;
                     })))
        |> String.concat "\n")
  in
  Printf.sprintf "%sdirective @%s%s%s on %s"
    (printDescription definition.description 0)
    definition.name arguments
    (if definition.repeatable then " repeatable" else "")
    (definition.locations
    |> List.map GenerateSchemaDirectiveUtils.locationToString
    |> String.concat " | ")

let printFieldArgument schemaState ~parentTypeName ~fieldName
    (argument : gqlArg) =
  Printf.sprintf "%s: %s%s%s%s" argument.name
    (graphqlTypeToString argument.typ)
    (match argument.defaultValue with
    | None -> ""
    | Some value -> " = " ^ constValueToString value)
    (printDeprecatedDirective argument.deprecationReason)
    (printDirectiveApplications schemaState
       (DirectiveArgumentDefinition
          {parentTypeName; fieldName; argumentName = argument.name}))

let printFieldArguments schemaState ~parentTypeName ~fieldName arguments =
  if arguments = [] then ""
  else if
    arguments
    |> List.exists (fun (argument : gqlArg) ->
        Option.is_some argument.description)
  then
    Printf.sprintf "(\n%s\n  )"
      (arguments
      |> List.map (fun (argument : gqlArg) ->
          (match argument.description with
            | None -> ""
            | Some description ->
              Printf.sprintf "    \"\"\"%s\"\"\"\n" description)
          ^ "    "
          ^ printFieldArgument schemaState ~parentTypeName ~fieldName argument)
      |> String.concat "\n")
  else
    Printf.sprintf "(%s)"
      (arguments
      |> List.map (printFieldArgument schemaState ~parentTypeName ~fieldName)
      |> String.concat ", ")

let printFields ~schemaState ~parentTypeName ~input fields =
  fields
  |> List.map (fun (f : gqlField) ->
      let args = GenerateSchemaUtils.onlyPrintableArgs f.args in
      Printf.sprintf "%s  %s%s: %s%s"
        (printDescription f.description 2)
        f.name
        (printFieldArguments schemaState ~parentTypeName ~fieldName:f.name args)
        (graphqlTypeToString f.typ
        ^
        if input then
          match f.defaultValue with
          | None -> ""
          | Some value -> " = " ^ constValueToString value
        else "")
        (printDeprecatedDirective f.deprecationReason
        ^ printDirectiveApplications schemaState
            (if input then
               DirectiveInputFieldDefinition
                 {inputObjectName = parentTypeName; fieldName = f.name}
             else DirectiveFieldDefinition {parentTypeName; fieldName = f.name})
        ))
  |> String.concat "\n"

let printSourceLoc = false

let printSourceLocDirective (typeLocation : typeLocation option) =
  if printSourceLoc = false then ""
  else
    match typeLocation with
    | Some (Concrete typeLocation) ->
      let start = typeLocation.loc |> Loc.start in

      let end_ = typeLocation.loc |> Loc.end_ in
      Printf.sprintf
        " @sourceLoc(fileUri: \"%s\", startLine: %i, startCol: %i, endLine: \
         %i, endCol: %i)"
        (typeLocation.fileUri |> Uri.toPath)
        (start |> fst) (start |> snd) (end_ |> fst) (end_ |> snd)
    | _ -> ""

let printInputObject schemaState (input : gqlInputObjectType) =
  Printf.sprintf "%sinput %s%s%s {\n%s\n}"
    (printDescription input.description 0)
    input.displayName
    (printSourceLocDirective
       (match input.typeLocation with
       | Some typeLocation -> Some (Concrete typeLocation)
       | None -> None))
    (printDirectiveApplications schemaState
       (DirectiveInputObject input.displayName))
    (printFields ~schemaState ~parentTypeName:input.displayName ~input:true
       input.fields)

let printInputUnion schemaState (input : gqlInputUnionType) =
  let input = inputUnionToInputObj input in
  Printf.sprintf "%sinput %s%s @oneOf%s {\n%s\n}"
    (printDescription input.description 0)
    input.displayName
    (printSourceLocDirective
       (match input.typeLocation with
       | Some typeLocation -> Some (Concrete typeLocation)
       | None -> None))
    (printDirectiveApplications schemaState
       (DirectiveInputObject input.displayName))
    (printFields ~schemaState ~parentTypeName:input.displayName ~input:true
       input.fields)

let printScalar schemaState (scalar : gqlScalar) =
  Printf.sprintf "%sscalar %s%s%s"
    (printDescription scalar.description 0)
    scalar.displayName
    (match scalar.specifiedByUrl with
    | None -> ""
    | Some url -> Printf.sprintf " @specifiedBy(url: \"%s\")" (Json.escape url))
    (printDirectiveApplications schemaState (DirectiveScalar scalar.displayName))

let printEnum schemaState (enum : gqlEnum) =
  Printf.sprintf "%senum %s%s%s {\n%s\n}"
    (printDescription enum.description 0)
    enum.displayName
    (printSourceLocDirective (Some enum.typeLocation))
    (printDirectiveApplications schemaState (DirectiveEnum enum.displayName))
    (enum.values
    |> List.map (fun (v : gqlEnumValue) ->
        Printf.sprintf "%s  %s%s"
          (printDescription v.description 2)
          v.value
          (printDeprecatedDirective v.deprecationReason
          ^ printDirectiveApplications schemaState
              (DirectiveEnumValue
                 {enumName = enum.displayName; valueName = v.value})))
    |> String.concat "\n")

let printUnion schemaState (union : gqlUnion) =
  Printf.sprintf "%sunion %s%s%s =\n%s\n"
    (printDescription union.description 0)
    union.displayName
    (printSourceLocDirective (Some union.typeLocation))
    (printDirectiveApplications schemaState (DirectiveUnion union.displayName))
    (union.types
    |> List.map (fun (v : gqlUnionMember) ->
        Printf.sprintf "  | %s%s"
          (match v.description with
          | None -> ""
          | Some desc -> Printf.sprintf "\"\"\"%s\"\"\" " desc)
          v.displayName)
    |> String.concat "\n")

let printInterface schemaState (intf : gqlInterface) =
  Printf.sprintf "%sinterface %s%s%s%s {\n%s\n}"
    (printDescription intf.description 0)
    intf.displayName
    (printImplements intf.interfaces)
    (printSourceLocDirective (Some (Concrete intf.typeLocation)))
    (printDirectiveApplications schemaState
       (DirectiveInterface intf.displayName))
    (printFields ~schemaState ~parentTypeName:intf.displayName ~input:false
       intf.fields)

let printObjectType schemaState (typ : gqlObjectType) =
  Printf.sprintf "%stype %s%s%s%s {\n%s\n}"
    (printDescription typ.description 0)
    typ.displayName
    (printImplements typ.interfaces)
    (printSourceLocDirective typ.typeLocation)
    (printDirectiveApplications schemaState (DirectiveObject typ.displayName))
    (printFields ~schemaState ~parentTypeName:typ.displayName ~input:false
       typ.fields)

let printSchemaDefinition schemaState (definition : gqlSchemaDefinition) =
  let operation operationName = function
    | None -> []
    | Some (typ : gqlObjectType) ->
      [Printf.sprintf "  %s: %s" operationName typ.displayName]
  in
  Printf.sprintf "%sschema%s {\n%s\n}"
    (printDescription definition.description 0)
    (printDirectiveApplications schemaState DirectiveSchema)
    (operation "query" schemaState.query
     @ operation "mutation" schemaState.mutation
     @ operation "subscription" schemaState.subscription
    |> String.concat "\n")

let printSchemaSDL (schemaState : schemaState) =
  let code = Buffer.create 16384 in
  let addWithNewLine text =
    Buffer.add_string code text;
    Buffer.add_char code '\n'
  in
  let addSection text = addWithNewLine (text ^ "\n") in

  if printSourceLoc then
    addSection
      "directive @sourceLoc(fileUri: String!, startLine: Int!, startCol: Int!, \
       startLine: Int!, startCol: Int!) on FIELD_DEFINITION | OBJECT | ENUM | \
       UNION | INPUT_OBJECT | INPUT_FIELD_DEFINITION | INTERFACE | SCALAR | \
       ARGUMENT_DEFINITION";

  schemaState.directiveDefinitions
  |> iterHashtblAlphabetically (fun _ definition ->
      addSection (printDirectiveDefinition schemaState definition));

  (match schemaState.schemaDefinition with
  | None -> ()
  | Some definition -> addSection (printSchemaDefinition schemaState definition));

  schemaState.scalars
  |> iterHashtblAlphabetically (fun _ (scalar : gqlScalar) ->
      addSection (printScalar schemaState scalar));

  schemaState.enums
  |> iterHashtblAlphabetically (fun _name (enum : gqlEnum) ->
      addSection (printEnum schemaState enum));

  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
      addSection (printUnion schemaState union));

  schemaState.inputObjects
  |> iterHashtblAlphabetically (fun _name (input : gqlInputObjectType) ->
      addSection (printInputObject schemaState input));

  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (input : gqlInputUnionType) ->
      addSection (printInputUnion schemaState input));

  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (intf : gqlInterface) ->
      addSection (printInterface schemaState intf));

  schemaState.types
  |> iterHashtblAlphabetically (fun _name (typ : gqlObjectType) ->
      addSection (printObjectType schemaState typ));
  String.trim (Buffer.contents code) ^ "\n"
