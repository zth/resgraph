open GenerateSchemaTypes

let addDiagnostic schemaState ~diagnostic =
  schemaState.diagnostics <- diagnostic :: schemaState.diagnostics

let extractInterfacesImplemented (payload : Parsetree.payload) =
  match payload with
  | PStr structure ->
    structure
    |> List.concat_map (fun item ->
           match item.Parsetree.pstr_desc with
           | Pstr_eval ({pexp_desc = Pexp_record (fields, _spread)}, _) ->
             fields
             |> List.concat_map (fun (name, exp) ->
                    match (name, exp) with
                    | ( {Location.txt = Longident.Lident "interfaces"},
                        {Parsetree.pexp_desc = Pexp_array items} ) ->
                      items
                      |> List.filter_map (fun e ->
                             match e.Parsetree.pexp_desc with
                             | Pexp_ident lident -> Some lident
                             | _ -> None)
                    | _ -> [])
           | _ -> [])
  | _ -> []

let validAttributes =
  [
    ("gql.type", "Indicates that the annotated record is a GraphQL Object Type.");
    ( "gql.interface",
      "Indicates that the annotated record is a GraphQL interface." );
    ("gql.interfaceResolver", "");
    ("gql.field", "");
    ("gql.enum", "");
    ("gql.union", "");
    ("gql.inputObject", "");
  ]

let extractGqlAttribute ~(schemaState : GenerateSchemaTypes.schemaState)
    ~(env : SharedTypes.QueryEnv.t) (attributes : Parsetree.attributes) =
  attributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
         match String.split_on_char '.' name.txt with
         | ["gql"; "type"] ->
           let interfaces = extractInterfacesImplemented payload in
           Some (ObjectType {interfaces})
         | ["gql"; "interface"] ->
           let interfaces = extractInterfacesImplemented payload in
           Some (Interface {interfaces})
         | ["gql"; "interfaceResolver"] -> (
           match payload with
           | PStr
               [
                 {
                   pstr_desc =
                     Pstr_eval
                       ( {
                           pexp_desc =
                             Pexp_constant (Pconst_string (interfaceId, _));
                         },
                         _ );
                 };
               ] ->
             Some (InterfaceResolver {interfaceId})
           | _ ->
             schemaState
             |> addDiagnostic
                  ~diagnostic:
                    {
                      loc = name.loc;
                      fileUri = env.file.uri;
                      message =
                        Printf.sprintf
                          "`%s` is annotated as @gql.interfaceResolver but did \
                           not have a string literal as payload."
                          name.txt;
                    };
             None)
         | ["gql"; "field"] -> Some Field
         | ["gql"; "enum"] -> Some Enum
         | ["gql"; "union"] -> Some Union
         | ["gql"; "inputObject"] -> Some InputObject
         | "gql" :: _ ->
           schemaState
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

let getFieldAttributeFromRawAttributes ~env ~schemaState attributes =
  attributes |> extractGqlAttribute ~env ~schemaState |> getFieldAttribute

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

type expectedType =
  | ObjectType
  | InputObject
  | Field
  | Enum
  | Union
  | Interface

(** Figures out the path to a target type in a file. *)
let rec findModulePathOfType ~schemaState ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : expectedType) ?(modulePath = [])
    ?(structure = env.file.structure) name =
  let open SharedTypes.Module in
  structure.items
  |> List.find_map (fun (item : item) ->
         match
           ( item.kind,
             expectedType,
             item.attributes |> extractGqlAttribute ~env ~schemaState )
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
         | Type ({kind = Record _}, _), Interface, Some (Interface _)
           when item.name = name ->
           Some modulePath
         | Type ({kind = Record _}, _), InputObject, Some InputObject
           when item.name = name ->
           Some modulePath
         | Module (Structure structure), _, _ ->
           name
           |> findModulePathOfType ~env ~expectedType
                ~modulePath:(structure.name :: modulePath)
                ~structure ~schemaState
         | _ -> None)

let findTypeLocation ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : expectedType) ~schemaState ~loc name =
  let typeLocation : typeLocation =
    {
      fileName = env.file.moduleName;
      modulePath = [];
      typeName = name;
      loc;
      fileUri = env.file.uri;
    }
  in
  match findModulePathOfType ~schemaState ~env ~expectedType name with
  | None ->
    schemaState
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
                 | Field -> "field"
                 | Interface -> "interface")
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

let noticeObjectType ~env ~loc ~debug ~schemaState ~displayName ~interfaces
    ?description ~makeFields typeName =
  if Hashtbl.mem schemaState.types typeName then ()
  else (
    if debug then Printf.printf "noticing %s\n" typeName;
    let typ : gqlObjectType =
      {
        id = typeName;
        displayName;
        fields = makeFields ();
        description;
        interfaces;
        typeLocation =
          findTypeLocation ~schemaState typeName ~env ~loc
            ~expectedType:ObjectType;
      }
    in
    Hashtbl.add schemaState.types typeName typ;
    match typeName with
    | "query" -> schemaState.query <- Some typ
    | "mutation" -> schemaState.mutation <- Some typ
    | "subscription" -> schemaState.subscription <- Some typ
    | _ -> ())

let addEnum id ~(makeEnum : unit -> gqlEnum) ~debug ~schemaState =
  if Hashtbl.mem schemaState.enums id then ()
  else (
    if debug then Printf.printf "Adding enum %s\n" id;
    Hashtbl.replace schemaState.enums id (makeEnum ()))

let addUnion id ~(makeUnion : unit -> gqlUnion) ~debug ~schemaState =
  if Hashtbl.mem schemaState.unions id then ()
  else (
    if debug then Printf.printf "Adding union %s\n" id;
    Hashtbl.replace schemaState.unions id (makeUnion ()))

let addInterface id ~(makeInterface : unit -> gqlInterface) ~debug ~schemaState
    =
  if Hashtbl.mem schemaState.interfaces id then ()
  else (
    if debug then Printf.printf "Adding interface %s\n" id;
    Hashtbl.replace schemaState.interfaces id (makeInterface ()))

let addInputObject id ~(makeInputObject : unit -> gqlInputObjectType) ~debug
    ~schemaState =
  if Hashtbl.mem schemaState.inputObjects id then ()
  else (
    if debug then Printf.printf "Adding input object %s\n" id;
    Hashtbl.replace schemaState.inputObjects id (makeInputObject ()))

let addFieldToObjectType ~env ~loc ~field ~schemaState typeName =
  let typ : gqlObjectType =
    match Hashtbl.find_opt schemaState.types typeName with
    | None ->
      {
        id = typeName;
        displayName = capitalizeFirstChar typeName;
        fields = [field];
        interfaces = [];
        description = field.description;
        typeLocation =
          findTypeLocation ~schemaState typeName ~env ~loc
            ~expectedType:ObjectType;
      }
    | Some typ -> {typ with fields = field :: typ.fields}
  in
  Hashtbl.replace schemaState.types typeName typ

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

let printDiagnostic (diagnostic : diagnostic) =
  Printf.sprintf
    "      {\n\
    \        \"range\": %s,\n\
    \        \"file\": \"%s\",\n\
    \        \"message\": \"%s\"\n\
    \      }"
    (diagnostic.loc |> Utils.cmtLocToRange |> Protocol.stringifyRange)
    (diagnostic.fileUri |> Uri.toPath |> Json.escape)
    (diagnostic.message |> Json.escape)

let pathIdentToList (p : Path.t) =
  let rec pathIdentToListInner ?(acc = []) (p : Path.t) =
    match p with
    | Pident {name} -> name :: acc
    | Pdot (nextPath, id, _) -> [id] @ pathIdentToListInner ~acc nextPath
    | Papply _ -> acc
  in
  let lst = pathIdentToListInner p in
  lst |> List.rev

let processSchema (schemaState : schemaState) =
  let processedSchema = {interfaceImplementedBy = Hashtbl.create 10} in
  schemaState.types
  |> Hashtbl.iter (fun _name (t : gqlObjectType) ->
         t.interfaces
         |> List.iter (fun (i : gqlInterfaceIdentifier) ->
                match
                  Hashtbl.find_opt processedSchema.interfaceImplementedBy i.id
                with
                | None ->
                  Hashtbl.add processedSchema.interfaceImplementedBy i.id
                    [ObjectType (Hashtbl.find schemaState.types t.id)]
                | Some item ->
                  Hashtbl.replace processedSchema.interfaceImplementedBy i.id
                    (ObjectType (Hashtbl.find schemaState.types t.id) :: item)));
  schemaState.interfaces
  |> Hashtbl.iter (fun _name (t : gqlInterface) ->
         t.interfaces
         |> List.iter (fun (i : gqlInterfaceIdentifier) ->
                match
                  Hashtbl.find_opt processedSchema.interfaceImplementedBy i.id
                with
                | None ->
                  Hashtbl.add processedSchema.interfaceImplementedBy i.id
                    [Interface (Hashtbl.find schemaState.interfaces t.id)]
                | Some item ->
                  Hashtbl.replace processedSchema.interfaceImplementedBy i.id
                    (Interface (Hashtbl.find schemaState.interfaces t.id)
                    :: item)));
  processedSchema

(** Some arguments aren't intended to be printed in the `args` list, like
  `InjectContext` which controls injecting context into the resolver. *)
let onlyPrintableArgs (args : gqlArg list) =
  args
  |> List.filter (fun (arg : gqlArg) ->
         if arg.typ = InjectContext then false else true)

let isFileContentsTheSame filePath s =
  let ic = open_in filePath in
  let fileLength = in_channel_length ic in
  let len = String.length s in
  if fileLength <> len then (
    close_in ic;
    false)
  else
    let contents = really_input_string ic fileLength in
    close_in ic;
    contents = s

let writeIfHasChanges path contents ~debug =
  if debug then ()
  else if isFileContentsTheSame path contents then ()
  else
    let oc = open_out path in
    output_string oc contents;
    close_out oc

let getStateFilePath (full : SharedTypes.full) =
  full.package.rootPath ^ "/lib/.resgraphState.marshal"

let writeStateFile ~full ~schemaState ~processedSchema =
  let s = Marshal.to_bytes (schemaState, processedSchema) [Compat_32] in
  let ch = open_out_bin (getStateFilePath full) in
  output_bytes ch s

let readStateFile ~full =
  let ch = open_in_bin (getStateFilePath full) in
  let s : GenerateSchemaTypes.schemaState * GenerateSchemaTypes.processedSchema
      =
    Marshal.from_channel ch
  in
  s
