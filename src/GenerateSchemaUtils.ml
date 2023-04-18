open GenerateSchemaTypes

let addDiagnostic schemaState ~diagnostic =
  schemaState.diagnostics <- diagnostic :: schemaState.diagnostics

let findInterfacesOfType code ~schemaState =
  let {Res_driver.parsetree = structure} =
    Res_driver.parseImplementationFromSource ~forPrinter:true ~source:code
      ~displayFilename:"-"
  in
  match structure with
  | [{pstr_desc = Pstr_type (_, [{ptype_kind = Ptype_record fields}])}] -> (
    match
      fields
      |> List.filter_map (fun (field : Parsetree.label_declaration) ->
             match field with
             | {
              pld_name = {txt = "..."};
              pld_type = {ptyp_desc = Ptyp_constr (loc, _)};
             } ->
               let interfaceName = loc.txt |> Longident.last in
               if Hashtbl.mem schemaState.interfaces interfaceName then
                 Some interfaceName
               else None
             | _ -> None)
    with
    | [] -> None
    | v -> Some v)
  | _ -> None

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
         | ["gql"; "type"] -> Some ObjectType
         | ["gql"; "interface"] -> Some Interface
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
         | Type ({kind = Record _}, _), ObjectType, Some ObjectType
           when item.name = name ->
           Some modulePath
         | Type ({kind = Record _}, _), Interface, Some Interface
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

let noticeObjectType ~env ~loc ~debug ~schemaState ~displayName ?description
    ~makeFields typeName =
  if Hashtbl.mem schemaState.types typeName then ()
  else (
    if debug then Printf.printf "noticing %s\n" typeName;
    let typ : gqlObjectType =
      {
        id = typeName;
        displayName;
        fields = makeFields ();
        description;
        interfaces = [];
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

let addFieldToInterfaceType ~env ~loc ~field ~schemaState typeName =
  let typ : gqlInterface =
    match Hashtbl.find_opt schemaState.interfaces typeName with
    | None ->
      {
        id = typeName;
        displayName = capitalizeFirstChar typeName;
        fields = [field];
        interfaces = [];
        description = field.description;
        typeLocation =
          findTypeLocation ~schemaState typeName ~env ~loc
            ~expectedType:Interface;
      }
    | Some typ -> {typ with fields = field :: typ.fields}
  in
  Hashtbl.replace schemaState.interfaces typeName typ

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

type positionItemType = ObjectType | Interface

type positionToRead = {
  id: string;
  position: Pos.t * Pos.t;
  typ: positionItemType;
}

type implementsInterface = {
  id: string;
  interfaces: string list;
  typ: positionItemType;
}

let spreadRegexp = Str.regexp_string "..."

let hasSpreadText str =
  try
    let _ = Str.search_forward spreadRegexp str 0 in
    true
  with Not_found -> false

let processSchema (schemaState : schemaState) =
  let processedSchema = {interfaceImplementedBy = Hashtbl.create 10} in
  let positionsToRead = Hashtbl.create 10 in

  (* Figure out all files that needs reading to check for interface spreads *)
  schemaState.types
  |> Hashtbl.iter (fun _name (t : gqlObjectType) ->
         let fileUri = t.typeLocation.fileUri |> Uri.toPath in
         match Hashtbl.find_opt positionsToRead fileUri with
         | None ->
           Hashtbl.add positionsToRead fileUri
             [
               {
                 id = t.id;
                 position =
                   ( t.typeLocation.loc |> Loc.start,
                     t.typeLocation.loc |> Loc.end_ );
                 typ = ObjectType;
               };
             ]
         | Some existingEntries ->
           Hashtbl.replace positionsToRead fileUri
             ({
                id = t.id;
                position =
                  ( t.typeLocation.loc |> Loc.start,
                    t.typeLocation.loc |> Loc.end_ );
                typ = ObjectType;
              }
             :: existingEntries));

  schemaState.interfaces
  |> Hashtbl.iter (fun _name (t : gqlInterface) ->
         let fileUri = t.typeLocation.fileUri |> Uri.toPath in
         match Hashtbl.find_opt positionsToRead fileUri with
         | None ->
           Hashtbl.add positionsToRead fileUri
             [
               {
                 id = t.id;
                 position =
                   ( t.typeLocation.loc |> Loc.start,
                     t.typeLocation.loc |> Loc.end_ );
                 typ = ObjectType;
               };
             ]
         | Some existingEntries ->
           Hashtbl.replace positionsToRead fileUri
             ({
                id = t.id;
                position =
                  ( t.typeLocation.loc |> Loc.start,
                    t.typeLocation.loc |> Loc.end_ );
                typ = ObjectType;
              }
             :: existingEntries));

  (* Read all noted positions so we can check them for spreads. *)
  positionsToRead
  |> Hashtbl.iter (fun fileUri entries ->
         let entries =
           entries
           |> List.sort (fun (a : positionToRead) (b : positionToRead) ->
                  compare (a.position |> fst) (b.position |> fst))
         in
         let fileChannel = open_in fileUri in
         let rec loop ?(hasSpread = false) lineNumber acc
             ({position = (startLine, startCol), (endLine, endCol)} as entry) =
           try
             let line = input_line fileChannel in
             if lineNumber > endLine then
               (String.concat "" (List.rev acc), lineNumber + 1, hasSpread)
             else if lineNumber >= startLine && lineNumber <= endLine then
               loop
                 ~hasSpread:(hasSpread || hasSpreadText line)
                 (lineNumber + 1) (line :: acc) entry
             else if lineNumber = startLine then
               let start = if startLine = endLine then startCol else 0 in
               let part = String.sub line start (endCol - start) in
               loop
                 ~hasSpread:(hasSpread || hasSpreadText line)
                 (lineNumber + 1) (part :: acc) entry
             else
               loop
                 ~hasSpread:(hasSpread || hasSpreadText line)
                 (lineNumber + 1) acc entry
           with End_of_file ->
             close_in fileChannel;
             (String.concat "" (List.rev acc), lineNumber + 1, hasSpread)
         in
         let lastEndlingLine = ref 0 in
         entries
         |> List.iter (fun entry ->
                let typeStr, endingLineNum, hasSpread =
                  loop !lastEndlingLine [] entry
                in
                (* Check for interfaces *)
                (if hasSpread then
                 match findInterfacesOfType ~schemaState typeStr with
                 | None -> ()
                 | Some implementsInterfaces -> (
                   (* Add all found interfaces to relevant types or interfaces. *)
                   match entry.typ with
                   | ObjectType ->
                     Hashtbl.replace schemaState.types entry.id
                       {
                         (Hashtbl.find schemaState.types entry.id) with
                         interfaces = implementsInterfaces;
                       };

                     (* Process each interface for this type *)
                     implementsInterfaces
                     |> List.iter (fun intfId ->
                            let interface =
                              Hashtbl.find schemaState.interfaces intfId
                            in
                            let typ = Hashtbl.find schemaState.types entry.id in
                            let doesNotHaveField name =
                              typ.fields
                              |> List.exists (fun (field : gqlField) ->
                                     field.name = name)
                              = false
                            in
                            (* Add relevant fields from interface to the type implementing it *)
                            Hashtbl.replace schemaState.types entry.id
                              {
                                typ with
                                fields =
                                  typ.fields
                                  @ (interface.fields
                                    |> List.filter (fun (field : gqlField) ->
                                           doesNotHaveField field.name));
                              };

                            (* Map interface as implemented by this type *)
                            match
                              Hashtbl.find_opt
                                processedSchema.interfaceImplementedBy intfId
                            with
                            | None ->
                              Hashtbl.add processedSchema.interfaceImplementedBy
                                intfId
                                [
                                  ObjectType
                                    (Hashtbl.find schemaState.types entry.id);
                                ]
                            | Some item ->
                              Hashtbl.replace
                                processedSchema.interfaceImplementedBy intfId
                                (ObjectType
                                   (Hashtbl.find schemaState.types entry.id)
                                :: item))
                   | Interface ->
                     Hashtbl.replace schemaState.interfaces entry.id
                       {
                         (Hashtbl.find schemaState.interfaces entry.id) with
                         interfaces = implementsInterfaces;
                       };
                     implementsInterfaces
                     |> List.iter (fun intfId ->
                            match
                              Hashtbl.find_opt
                                processedSchema.interfaceImplementedBy intfId
                            with
                            | None ->
                              Hashtbl.add processedSchema.interfaceImplementedBy
                                intfId
                                [
                                  Interface
                                    (Hashtbl.find schemaState.interfaces
                                       entry.id);
                                ]
                            | Some item ->
                              Hashtbl.replace
                                processedSchema.interfaceImplementedBy intfId
                                (Interface
                                   (Hashtbl.find schemaState.interfaces entry.id)
                                :: item))));
                lastEndlingLine := endingLineNum);
         close_in fileChannel);

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

let gqlRegexp = Str.regexp_string "@gql."

let hasGqlAttribute str =
  try
    let _ = Str.search_forward gqlRegexp str 0 in
    true
  with Not_found -> false

let rec readLinesUntilValue fileChannel =
  try
    let line = input_line fileChannel in
    if hasGqlAttribute line then (
      close_in fileChannel;
      true)
    else readLinesUntilValue fileChannel
  with End_of_file ->
    close_in fileChannel;
    false

let fileHasGqlAttribute filePath =
  let fileChannel = open_in filePath in
  readLinesUntilValue fileChannel

let writeIfHasChanges path contents ~debug =
  if debug then ()
  else if isFileContentsTheSame path contents then ()
  else
    let oc = open_out path in
    output_string oc contents;
    close_out oc

let getStateFilePath (package : SharedTypes.package) =
  package.rootPath ^ "/lib/.resgraphState.marshal"

let writeStateFile ~package ~schemaState ~processedSchema =
  let s = Marshal.to_bytes (schemaState, processedSchema) [Compat_32] in
  let ch = open_out_bin (getStateFilePath package) in
  output_bytes ch s

let readStateFile ~package =
  let ch = open_in_bin (getStateFilePath package) in
  let s : GenerateSchemaTypes.schemaState * GenerateSchemaTypes.processedSchema
      =
    Marshal.from_channel ch
  in
  s

let iterHashtblAlphabetically fn hashtbl =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) hashtbl []
  |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
  |> List.iter (fun (k, v) -> fn k v)
