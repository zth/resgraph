open GenerateSchemaTypes

let extractGqlAttribute (attributes : Parsetree.attributes) =
  attributes
  |> List.find_map (fun ((name, _payload) : Parsetree.attribute) ->
         match String.split_on_char '.' name.txt with
         | ["gql"; "type"] -> Some ObjectType
         | ["gql"; "field"] -> Some Field
         | ["gql"; "enum"] -> Some Enum
         | ["gql"; "union"] -> Some Union
         | ["gql"; "inputObject"] -> Some InputObject
         | "gql" :: _ -> (* TODO: Warn about invalid gql annotation*) None
         | _ -> None)

let getFieldAttribute gqlAttribute =
  match gqlAttribute with
  | Some Field -> Some gqlAttribute
  | _ -> None

let getFieldAttributeFromRawAttributes attributes =
  attributes |> extractGqlAttribute |> getFieldAttribute

let formatCode code =
  let {Res_driver.parsetree = structure; comments; diagnostics} =
    Res_driver.parseImplementationFromSource ~forPrinter:true ~source:code
      ~displayFilename:"Schema.res"
  in
  let printed =
    Res_printer.printImplementation ~width:!Res_cli.ResClflags.width ~comments
      structure
  in
  if List.length diagnostics > 0 then
    "\n\n== SYNTAX ERRORS ==\n"
    ^ (Diagnostics.get_diagnostics diagnostics |> String.concat "\n\n")
    ^ "\n\n== RAW CODE ==\n" ^ code ^ "\n\n === END ==\n" ^ printed
  else printed

exception Module_path_cannot_be_determined

let rec findModulePathOfType ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : gqlAttributes) ?(modulePath = [])
    ?(structure = env.file.structure) name =
  let open SharedTypes.Module in
  structure.items
  |> List.find_map (fun (item : item) ->
         match
           (item.kind, expectedType, item.attributes |> extractGqlAttribute)
         with
         | Type ({kind = Variant _}, _), Enum, Some Enum when item.name = name
           ->
           Some modulePath
         | Type ({kind = Variant _}, _), Union, Some Union when item.name = name
           ->
           Some modulePath
         | Type ({kind = Record _}, _), ObjectType, Some ObjectType
           when item.name = name ->
           Some modulePath
         | Type ({kind = Record _}, _), InputObject, Some InputObject
           when item.name = name ->
           Some modulePath
         | Module (Structure structure), _, _ ->
           name
           |> findModulePathOfType ~env ~expectedType
                ~modulePath:(structure.name :: modulePath)
                ~structure
         | _ -> None)

let findTypeLocation ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : gqlAttributes) ~loc name =
  match findModulePathOfType ~env ~expectedType name with
  | None -> raise Module_path_cannot_be_determined
  | Some modulePath ->
    let typeLocation : typeLocation =
      {fileName = env.file.moduleName; modulePath; typeName = name; loc}
    in
    typeLocation

let typeLocationToAccessor (typeLocation : typeLocation) =
  [typeLocation.fileName] @ typeLocation.modulePath @ [typeLocation.typeName]
  |> String.concat "."

let capitalizeFirstChar s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s

(* TODO: Capitalize names here, but also track the underlying typename so we can look that up. *)
let graphqlTypeFromItem (item : SharedTypes.Type.t) =
  let gqlAttribute = extractGqlAttribute item.attributes in
  match (gqlAttribute, item) with
  | Some ObjectType, {kind = Record _; name} ->
    Some (GraphQLObjectType {id = name; displayName = capitalizeFirstChar name})
  | Some InputObject, {kind = Record _; name} ->
    Some
      (GraphQLInputObject {id = name; displayName = capitalizeFirstChar name})
  | Some Enum, {kind = Variant _; name} ->
    Some (GraphQLEnum {id = name; displayName = capitalizeFirstChar name})
  | Some Union, {kind = Variant _; name} ->
    Some (GraphQLUnion {id = name; displayName = capitalizeFirstChar name})
  | _ -> None

let noticeObjectType ~env ~loc ~state ?description typeName =
  if Hashtbl.mem state.types typeName then
    Printf.printf "already seen %s\n" typeName
  else (
    Printf.printf "noticing %s\n" typeName;
    Hashtbl.add state.types typeName
      {
        id = typeName;
        displayName = capitalizeFirstChar typeName;
        fields = [];
        description;
        typeLocation =
          findTypeLocation typeName ~env ~loc ~expectedType:ObjectType;
      })

let addEnum enumName ~(enum : gqlEnum) ~state =
  Printf.printf "Adding enum %s\n" enumName;
  Hashtbl.replace state.enums enumName enum

let addUnion (union : gqlUnion) ~state =
  Printf.printf "Adding union %s\n" union.id;
  Hashtbl.replace state.unions union.id union

let addInputObject (obj : gqlInputObjectType) ~state =
  Printf.printf "Adding input object %s\n" obj.id;
  Hashtbl.replace state.inputObjects obj.id obj

let addFieldToObjectType ?description ~env ~loc ~field ~state typeName =
  let typ : gqlObjectType =
    match Hashtbl.find_opt state.types typeName with
    | None ->
      {
        id = typeName;
        displayName = capitalizeFirstChar typeName;
        fields = [field];
        description;
        typeLocation =
          findTypeLocation typeName ~env ~loc ~expectedType:ObjectType;
      }
    | Some typ -> {typ with fields = field :: typ.fields}
  in
  Hashtbl.replace state.types typeName typ

let getOrRaise opt msg =
  match opt with
  | None -> raise (Failure msg)
  | Some v -> v

let undefinedOrValueAsString v =
  match v with
  | None -> "?(None)"
  | Some v -> Printf.sprintf "\"%s\"" v

let trimString str =
  let isSpace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  let len = String.length str in
  let rec find_start i =
    if i >= len then len
    else if not (isSpace str.[i]) then i
    else find_start (i + 1)
  in
  let rec find_end i =
    if i < 0 then 0
    else if not (isSpace str.[i]) then i + 1
    else find_end (i - 1)
  in
  let start = find_start 0 in
  let stop = find_end (len - 1) in
  if start >= stop then "" else String.sub str start (stop - start)

let attributesToDocstring attributes =
  match ProcessAttributes.findDocAttribute attributes with
  | None -> None
  | Some doc -> Some (doc |> trimString)

let findContextArgName (args : gqlArg list) =
  args
  |> List.find_map (fun (arg : gqlArg) ->
         match arg.typ with
         | InjectContext -> Some arg.name
         | _ -> None)

let rec dumpContents (graphqlType : graphqlType) =
  match graphqlType with
  | Nullable inner -> Printf.sprintf "Nullable(%s)" (dumpContents inner)
  | List inner -> Printf.sprintf "List(%s)" (dumpContents inner)
  | RescriptNullable inner -> dumpContents inner
  | _ -> "v"

let rec typeNeedsConversion (graphqlType : graphqlType) =
  match graphqlType with
  | List inner -> typeNeedsConversion inner
  | Nullable _ | RescriptNullable _ | GraphQLInputObject _ -> true
  | _ -> false

let rec generateConverter lastValue (graphqlType : graphqlType) =
  match graphqlType with
  | Nullable inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf
        "(switch %s->Js.Nullable.toOption { | None => None | Some(v) => \
         %s->Some})"
        lastValue innerConverter
    else Printf.sprintf "(%s->Js.Nullable.toOption)" lastValue
  | List inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf "(%s->Js.Array2.map(v => %s))" lastValue innerConverter
    else lastValue
  | RescriptNullable inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf "(%s->Js.Nullable.bind((. v) => %s))" lastValue
        innerConverter
    else lastValue
  | GraphQLInputObject {displayName} ->
    Printf.sprintf
      "applyConversionToInputObject(%s, input_%s_conversionInstructions)"
      lastValue displayName
  | _ -> lastValue

let printInputObjectAssets (inputObject : gqlInputObjectType) =
  Printf.sprintf
    "let _ = input_%s_conversionInstructions->Js.Array2.pushMany([%s]);"
    inputObject.displayName
    (inputObject.fields
    |> List.filter_map (fun (field : gqlField) ->
           let converter = generateConverter "v" field.typ in
           if converter = "v" then None
           else
             Some
               (Printf.sprintf
                  "(\"%s\", makeInputObjectFieldConverterFn((v) => %s))"
                  field.name converter))
    |> String.concat ", ")
