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
  | InjectInterfaceTypename intfId ->
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

let printFields fields =
  fields
  |> List.map (fun (f : gqlField) ->
         let args = GenerateSchemaUtils.onlyPrintableArgs f.args in
         Printf.sprintf "%s  %s%s: %s%s"
           (printDescription f.description 2)
           f.name
           (if List.length args > 0 then
              Printf.sprintf "(%s)"
                (args
                |> List.map (fun (arg : gqlArg) ->
                       Printf.sprintf "%s: %s" arg.name
                         (graphqlTypeToString arg.typ))
                |> String.concat ", ")
            else "")
           (graphqlTypeToString f.typ)
           (printDeprecatedDirective f.deprecationReason))
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

let printInputObject (input : gqlInputObjectType) =
  Printf.sprintf "%sinput %s%s {\n%s\n}"
    (printDescription input.description 0)
    input.displayName
    (printSourceLocDirective
       (match input.typeLocation with
       | Some typeLocation -> Some (Concrete typeLocation)
       | None -> None))
    (printFields input.fields)

let printInputUnion (input : gqlInputUnionType) =
  let input = inputUnionToInputObj input in
  Printf.sprintf "%sinput %s%s @oneOf {\n%s\n}"
    (printDescription input.description 0)
    input.displayName
    (printSourceLocDirective
       (match input.typeLocation with
       | Some typeLocation -> Some (Concrete typeLocation)
       | None -> None))
    (printFields input.fields)

let printScalar (scalar : gqlScalar) =
  Printf.sprintf "%sscalar %s"
    (printDescription scalar.description 0)
    scalar.displayName

let printEnum (enum : gqlEnum) =
  Printf.sprintf "%senum %s%s {\n%s\n}"
    (printDescription enum.description 0)
    enum.displayName
    (printSourceLocDirective (Some enum.typeLocation))
    (enum.values
    |> List.map (fun (v : gqlEnumValue) ->
           Printf.sprintf "%s  %s%s"
             (printDescription v.description 2)
             v.value
             (printDeprecatedDirective v.deprecationReason))
    |> String.concat "\n")

let printUnion (union : gqlUnion) =
  Printf.sprintf "%sunion %s%s = \n%s\n"
    (printDescription union.description 0)
    union.displayName
    (printSourceLocDirective (Some union.typeLocation))
    (union.types
    |> List.map (fun (v : gqlUnionMember) ->
           Printf.sprintf "  | %s%s"
             (match v.description with
             | None -> ""
             | Some desc -> Printf.sprintf "\"\"\"%s\"\"\" " desc)
             v.displayName)
    |> String.concat "\n")

let printInterface (intf : gqlInterface) =
  Printf.sprintf "%sinterface %s%s%s {\n%s\n}"
    (printDescription intf.description 0)
    intf.displayName
    (printImplements intf.interfaces)
    (printSourceLocDirective (Some (Concrete intf.typeLocation)))
    (printFields intf.fields)

let printObjectType (typ : gqlObjectType) =
  Printf.sprintf "%stype %s%s%s {\n%s\n}"
    (printDescription typ.description 0)
    typ.displayName
    (printImplements typ.interfaces)
    (printSourceLocDirective typ.typeLocation)
    (printFields typ.fields)

let printSchemaSDL (schemaState : schemaState) =
  let code = ref "" in
  let addWithNewLine text = code := !code ^ text ^ "\n" in
  let addSection text = addWithNewLine (text ^ "\n") in

  if printSourceLoc then
    addSection
      "directive @sourceLoc(fileUri: String!, startLine: Int!, startCol: Int!, \
       startLine: Int!, startCol: Int!) on FIELD_DEFINITION | OBJECT | ENUM | \
       UNION | INPUT_OBJECT | INPUT_FIELD_DEFINITION | INTERFACE | SCALAR | \
       ARGUMENT_DEFINITION";

  schemaState.scalars
  |> iterHashtblAlphabetically (fun _ (scalar : gqlScalar) ->
         addSection (printScalar scalar));

  schemaState.enums
  |> iterHashtblAlphabetically (fun _name (enum : gqlEnum) ->
         addSection (printEnum enum));

  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
         addSection (printUnion union));

  schemaState.inputObjects
  |> iterHashtblAlphabetically (fun _name (input : gqlInputObjectType) ->
         addSection (printInputObject input));

  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (input : gqlInputUnionType) ->
         addSection (printInputUnion input));

  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (intf : gqlInterface) ->
         addSection (printInterface intf));

  schemaState.types
  |> iterHashtblAlphabetically (fun _name (typ : gqlObjectType) ->
         addSection (printObjectType typ));
  !code
