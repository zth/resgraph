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

let noticeObjectType ~env ~loc ~state ?description ?makeFields typeName =
  if Hashtbl.mem state.types typeName then ()
  else (
    Printf.printf "noticing %s\n" typeName;
    let typ : gqlObjectType =
      {
        id = typeName;
        displayName = capitalizeFirstChar typeName;
        fields =
          (match makeFields with
          | None -> []
          | Some mk -> mk ());
        description;
        typeLocation =
          findTypeLocation typeName ~env ~loc ~expectedType:ObjectType;
      }
    in
    Hashtbl.add state.types typeName typ;
    match typeName with
    | "query" -> state.query <- Some typ
    | "mutation" -> state.mutation <- Some typ
    | "subscription" -> state.subscription <- Some typ
    | _ -> ())

let addEnum id ~(makeEnum : unit -> gqlEnum) ~state =
  if Hashtbl.mem state.enums id then ()
  else (
    Printf.printf "Adding enum %s\n" id;
    Hashtbl.replace state.enums id (makeEnum ()))

let addUnion id ~(makeUnion : unit -> gqlUnion) ~state =
  if Hashtbl.mem state.unions id then ()
  else (
    Printf.printf "Adding union %s\n" id;
    Hashtbl.replace state.unions id (makeUnion ()))

let addInputObject id ~(makeInputObject : unit -> gqlInputObjectType) ~state =
  if Hashtbl.mem state.inputObjects id then ()
  else (
    Printf.printf "Adding input object %s\n" id;
    Hashtbl.replace state.inputObjects id (makeInputObject ()))

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
        "(switch %s->Nullable.toOption { | None => None | Some(v) => %s->Some})"
        lastValue innerConverter
    else Printf.sprintf "(%s->Nullable.toOption)" lastValue
  | List inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf "(%s->Array.map(v => %s))" lastValue innerConverter
    else lastValue
  | RescriptNullable inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf "(%s->Nullable.map(v => %s))" lastValue innerConverter
    else lastValue
  | GraphQLInputObject {displayName} ->
    Printf.sprintf
      "%s->applyConversionToInputObject(input_%s_conversionInstructions)"
      lastValue displayName
  | _ -> lastValue

let printInputObjectAssets (inputObject : gqlInputObjectType) =
  Printf.sprintf "input_%s_conversionInstructions->Array.pushMany([%s]);"
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

let addDiagnostic state ~diagnostic =
  state.diagnostics <- diagnostic :: state.diagnostics

let printDiagnostic (diagnostic : diagnostic) =
  Printf.sprintf "Error at %s in file '%s':\n%s"
    (diagnostic.loc |> Loc.toString)
    (diagnostic.fileUri |> Uri.toString)
    diagnostic.message
