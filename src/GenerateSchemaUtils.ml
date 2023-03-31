open GenerateSchemaTypes

let addDiagnostic state ~diagnostic =
  state.diagnostics <- diagnostic :: state.diagnostics

let extractGqlAttribute ~state ~(env : SharedTypes.QueryEnv.t)
    (attributes : Parsetree.attributes) =
  attributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
         match String.split_on_char '.' name.txt with
         | ["gql"; "type"] ->
           let interfaces =
             match payload with
             | PStr structure ->
               structure
               |> List.filter_map (fun item ->
                      match item.Parsetree.pstr_desc with
                      | Pstr_eval
                          ({pexp_desc = Pexp_record (fields, _spread)}, _) ->
                        fields
                        |> List.find_map (fun (name, exp) ->
                               match (name, exp) with
                               | ( {Location.txt = Longident.Lident "interfaces"},
                                   {Parsetree.pexp_desc = Pexp_array items} ) ->
                                 items
                                 |> List.find_map (fun e ->
                                        match e.Parsetree.pexp_desc with
                                        | Pexp_ident lident -> Some lident
                                        | _ -> None)
                               | _ -> None)
                      | _ -> None)
             | _ -> []
           in
           Some (ObjectType {interfaces})
         | ["gql"; "field"] -> Some Field
         | ["gql"; "enum"] -> Some Enum
         | ["gql"; "union"] -> Some Union
         | ["gql"; "inputObject"] -> Some InputObject
         | "gql" :: _ ->
           state
           |> addDiagnostic
                ~diagnostic:
                  {
                    loc = name.loc;
                    fileUri = env.file.uri;
                    message =
                      Printf.sprintf
                        "`%s` is an invalid @gql annotation. Valid annotations \
                         are `@gql.type` for object types, `@gql.inputObject` \
                         for input objects, `@gql.enum` for enums, \
                         `@gql.union` for unions."
                        name.txt;
                  };
           None
         | _ -> None)

let getFieldAttribute gqlAttribute =
  match gqlAttribute with
  | Some Field -> Some gqlAttribute
  | _ -> None

let getFieldAttributeFromRawAttributes ~env ~state attributes =
  attributes |> extractGqlAttribute ~env ~state |> getFieldAttribute

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

type expectedType = ObjectType | InputObject | Field | Enum | Union

(** Figures out the path to a target type in a file. *)
let rec findModulePathOfType ~state ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : expectedType) ?(modulePath = [])
    ?(structure = env.file.structure) name =
  let open SharedTypes.Module in
  structure.items
  |> List.find_map (fun (item : item) ->
         match
           ( item.kind,
             expectedType,
             item.attributes |> extractGqlAttribute ~env ~state )
         with
         | Type ({kind = Variant _}, _), Enum, Some Enum when item.name = name
           ->
           Some modulePath
         | Type ({kind = Variant _}, _), Union, Some Union when item.name = name
           ->
           Some modulePath
         | Type ({kind = Record _}, _), ObjectType, Some (ObjectType _)
           when item.name = name ->
           Some modulePath
         | Type ({kind = Record _}, _), InputObject, Some InputObject
           when item.name = name ->
           Some modulePath
         | Module (Structure structure), _, _ ->
           name
           |> findModulePathOfType ~env ~expectedType
                ~modulePath:(structure.name :: modulePath)
                ~structure ~state
         | _ -> None)

let findTypeLocation ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : expectedType) ~state ~loc name =
  let typeLocation : typeLocation =
    {fileName = env.file.moduleName; modulePath = []; typeName = name; loc}
  in
  match findModulePathOfType ~state ~env ~expectedType name with
  | None ->
    state
    |> addDiagnostic
         ~diagnostic:
           {
             loc;
             fileUri = env.file.uri;
             message =
               Printf.sprintf
                 "Could not determine location of %s backed by type `%s` in %s."
                 (match expectedType with
                 | Union -> "union"
                 | Enum -> "enum"
                 | ObjectType -> "object type"
                 | InputObject -> "input object"
                 | Field -> "field")
                 name env.file.moduleName;
           };

    typeLocation
  | Some modulePath -> {typeLocation with modulePath}

let typeLocationToAccessor (typeLocation : typeLocation) =
  [typeLocation.fileName] @ typeLocation.modulePath @ [typeLocation.typeName]
  |> String.concat "."

let capitalizeFirstChar s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s

let noticeObjectType ~env ~loc ~state ~displayName ~interfaces ?description
    ~makeFields typeName =
  if Hashtbl.mem state.types typeName then ()
  else (
    Printf.printf "noticing %s\n" typeName;
    let typ : gqlObjectType =
      {
        id = typeName;
        displayName;
        fields = makeFields ();
        description;
        interfaces;
        typeLocation =
          findTypeLocation ~state typeName ~env ~loc ~expectedType:ObjectType;
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

let addFieldToObjectType ~env ~loc ~field ~state typeName =
  let typ : gqlObjectType =
    match Hashtbl.find_opt state.types typeName with
    | None ->
      {
        id = typeName;
        displayName = capitalizeFirstChar typeName;
        fields = [field];
        interfaces = [];
        description = field.description;
        typeLocation =
          findTypeLocation ~state typeName ~env ~loc ~expectedType:ObjectType;
      }
    | Some typ -> {typ with fields = field :: typ.fields}
  in
  Hashtbl.replace state.types typeName typ

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
  | List inner ->
    (* Lists might need conversion depending on the contents. *)
    typeNeedsConversion inner
  | Nullable _ | RescriptNullable _
  (* Input objects might need conversion because they might have null-enabled fields *)
  | GraphQLInputObject _ ->
    true
  | _ -> false

(* Runtime conversion for a structure of GraphQL types. *)
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

(* TODO: Copy printing from compiler (or just redo, like in the router...?)*)
let printDiagnostic (diagnostic : diagnostic) =
  Printf.sprintf "Error at %s in file '%s':\n%s"
    (diagnostic.loc |> Loc.toString)
    (diagnostic.fileUri |> Uri.toString)
    diagnostic.message

let pathIdentToList (p : Path.t) =
  let rec pathIdentToListInner ?(acc = []) (p : Path.t) =
    match p with
    | Pident {name} -> name :: acc
    | Pdot (nextPath, id, _) -> [id] @ pathIdentToListInner ~acc nextPath
    | Papply _ -> acc
  in
  let lst = pathIdentToListInner p in
  lst |> List.rev
