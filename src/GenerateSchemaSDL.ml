open GenerateSchemaTypes

(** More a PoC, since SDL didn't end up needed for now. *)

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
  | Nullable inner | RescriptNullable inner ->
    graphqlTypeToString ~nullable:true inner
  | List inner ->
    Printf.sprintf "[%s]%s" (graphqlTypeToString inner) nullableSuffix
  | GraphQLObjectType {displayName}
  | GraphQLInputObject {displayName}
  | GraphQLEnum {displayName}
  | GraphQLUnion {displayName}
  | GraphQLInterface {displayName} ->
    Printf.sprintf "%s%s" displayName nullableSuffix
  | InjectContext -> "Unknown"

let printImplements interfaces =
  if List.length interfaces > 0 then
    Printf.sprintf " implements %s"
      (interfaces
      |> List.map (fun id -> GenerateSchemaUtils.capitalizeFirstChar id)
      |> String.concat " & ")
  else ""

let printFields fields =
  fields
  |> List.map (fun (f : gqlField) ->
         let args = GenerateSchemaUtils.onlyPrintableArgs f.args in
         Printf.sprintf "  %s%s: %s" f.name
           (if List.length args > 0 then
            Printf.sprintf "(%s)"
              (args
              |> List.map (fun (arg : gqlArg) ->
                     Printf.sprintf "%s: %s" arg.name
                       (graphqlTypeToString arg.typ))
              |> String.concat ", ")
           else "")
           (graphqlTypeToString f.typ))
  |> String.concat "\n"

let printSourceLocDirective (typeLocation : typeLocation) =
  let start = typeLocation.loc |> Loc.start in
  let end_ = typeLocation.loc |> Loc.end_ in
  Printf.sprintf
    "@sourceLoc(fileUri: \"%s\", startLine: %i, startCol: %i, endLine: %i, \
     endCol: %i)"
    (typeLocation.fileUri |> Uri.toPath)
    (start |> fst) (start |> snd) (end_ |> fst) (end_ |> snd)

let printSchemaSDL (schemaState : schemaState) =
  let code = ref "" in
  let addWithNewLine text = code := !code ^ text ^ "\n" in
  let addSection text = addWithNewLine (text ^ "\n") in

  addSection
    "directive @sourceLoc(fileUri: String!, startLine: Int!, startCol: Int!, \
     startLine: Int!, startCol: Int!) on FIELD_DEFINITION | OBJECT | ENUM | \
     UNION | INPUT_OBJECT | INPUT_FIELD_DEFINITION | INTERFACE | SCALAR | \
     ARGUMENT_DEFINITION";

  schemaState.enums
  |> Hashtbl.iter (fun _name (enum : gqlEnum) ->
         addSection
           (Printf.sprintf "enum %s {\n%s\n}" enum.displayName
              (enum.values
              |> List.map (fun (v : gqlEnumValue) ->
                     Printf.sprintf "  %s" v.value)
              |> String.concat "\n")));

  schemaState.unions
  |> Hashtbl.iter (fun _name (union : gqlUnion) ->
         addSection
           (Printf.sprintf "union %s %s {\n%s\n}" union.displayName
              (printSourceLocDirective union.typeLocation)
              (union.types
              |> List.map (fun (v : gqlUnionMember) ->
                     Printf.sprintf "  %s" v.displayName)
              |> String.concat "\n")));

  schemaState.inputObjects
  |> Hashtbl.iter (fun _name (input : gqlInputObjectType) ->
         addSection
           (Printf.sprintf "input %s %s {\n%s\n}" input.displayName
              (printSourceLocDirective input.typeLocation)
              (printFields input.fields)));

  schemaState.interfaces
  |> Hashtbl.iter (fun _name (intf : gqlInterface) ->
         addSection
           (Printf.sprintf "interface %s%s %s {\n%s\n}" intf.displayName
              (printImplements intf.interfaces)
              (printSourceLocDirective intf.typeLocation)
              (printFields intf.fields)));

  schemaState.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
         addSection
           (Printf.sprintf "type %s%s %s {\n%s\n}" typ.displayName
              (printImplements typ.interfaces)
              (printSourceLocDirective typ.typeLocation)
              (printFields typ.fields)));
  !code
