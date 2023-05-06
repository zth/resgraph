open GenerateSchemaTypes
open GenerateSchemaDiagnostics

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
    ("gql.scalar", "");
  ]

let snippets =
  [
    ( "gql.type snippet - simple connection",
      "Boilerplate for creating a new simple GraphQL connection for pagination.",
      {|gql.type
/** An edge in a connection. */
type ${1:entity}Edge = ResGraph.Connections.edge<${1:entity}>

/** A connection to a list of items. */
@gql.type
type ${1:entity}Connection = ResGraph.Connections.connection<${1:entity}Edge>|}
    );
    ( "gql.type snippet - full connection",
      "Boilerplate for creating a new GraphQL connection for pagination.",
      {|gql.type
/** An edge in a connection. */
type ${1:entity}Edge = {
  /** A cursor for use in pagination. */
  @gql.field
  cursor: string,
  /** The item at the end of the edge. */
  @gql.field
  node: option<${1:entity}>
}

/** A connection to a list of items. */
@gql.type
type ${1:entity}Connection = {
  /** Information to aid in pagination. */
  @gql.field
  pageInfo: ResGraph.Connections.pageInfo,
  /** A list of edges. */
  @gql.field
  edges: option<array<option<${1:entity}Edge>>>
}|}
    );
    ( "gql.type snippet - field function on type",
      "Boilerplate for adding a new field to a type via a function.",
      {|gql.field
let ${1:fieldName} = (${2:entity}: ${2:entity}) => {
  ${0:Some(entity.prop)}
}|}
    );
  ]

let hasGqlAnnotation attributes =
  attributes
  |> List.exists (fun ((name, _payload) : Parsetree.attribute) ->
         Utils.startsWith name.txt "gql.")

let extractGqlAttribute ~(schemaState : GenerateSchemaTypes.schemaState)
    ~(env : SharedTypes.QueryEnv.t) (attributes : Parsetree.attributes) =
  attributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
         match String.split_on_char '.' name.txt with
         | ["gql"; "type"] -> Some ObjectType
         | ["gql"; "interface"] -> Some Interface
         | ["gql"; "scalar"] -> Some Scalar
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

let formatCode ~debug code =
  let {Res_driver.parsetree = structure; comments; diagnostics} =
    Res_driver.parseImplementationFromSource ~forPrinter:true ~source:code
      ~displayFilename:"Schema.res"
  in
  let printed =
    Res_printer.printImplementation ~width:!Res_cli.ResClflags.width ~comments
      structure
  in
  if List.length diagnostics > 0 then
    if debug then
      "\n\n== SYNTAX ERRORS ==\n"
      ^ (Diagnostics.get_diagnostics diagnostics |> String.concat "\n\n")
      ^ "\n\n== RAW CODE ==\n" ^ code ^ "\n\n === END ==\n" ^ printed
    else "/* Code had syntax errors. This is an internal ResGraph error. */"
  else printed

type expectedType =
  | ObjectType
  | InputObject
  | Field
  | Enum
  | Union
  | Interface
  | Scalar
  | ScalarT of {modulePath: string list}

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
         | ( Type ({name = "query" | "mutation"; kind = Abstract None}, _),
             ObjectType,
             Some ObjectType )
           when item.name = name ->
           Some modulePath
         | ( Type ({kind = Abstract (Some (_p, typeArgs))}, _),
             ObjectType,
             Some ObjectType )
           when item.name = name && List.length typeArgs > 0 ->
           Some modulePath
         | Type ({kind = Record _}, _), Interface, Some Interface
           when item.name = name ->
           Some modulePath
         | Type ({kind = Record _}, _), InputObject, Some InputObject
           when item.name = name ->
           Some modulePath
         | Type ({kind = Abstract (Some _)}, _), Scalar, Some Scalar
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
                 | Interface -> "interface"
                 | Scalar | ScalarT _ -> "scalar")
                 name env.file.moduleName;
           };

    typeLocation
  | Some modulePath -> {typeLocation with modulePath}

let typeLocationToAccessor (typeLocation : typeLocation) =
  [typeLocation.fileName] @ typeLocation.modulePath @ [typeLocation.typeName]
  |> String.concat "."

let typeLocationModuleToAccesor (typeLocation : typeLocation) endingPath =
  [typeLocation.fileName] @ typeLocation.modulePath @ endingPath
  |> String.concat "."

let capitalizeFirstChar s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s

let noticeObjectType ?(force = false) ?typeCreatorLocation ~env ~loc
    ~schemaState ~displayName ?description ~makeFields typeName =
  if Hashtbl.mem schemaState.types typeName && force = false then ()
  else
    (*Printf.printf "noticing %s\n" typeName;*)
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
        typeCreatorLocation;
      }
    in
    Hashtbl.add schemaState.types typeName typ;
    match typeName with
    | "query" -> schemaState.query <- Some typ
    | "mutation" -> schemaState.mutation <- Some typ
    | "subscription" -> schemaState.subscription <- Some typ
    | _ -> ()

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

let addScalar ~debug ~schemaState ?description ~typeLocation ?encoderDecoderLoc
    id =
  if Hashtbl.mem schemaState.scalars id then ()
  else (
    if debug then Printf.printf "Adding scalar %s\n" id;
    Hashtbl.replace schemaState.scalars id
      {
        id;
        displayName = capitalizeFirstChar id;
        description;
        typeLocation;
        specifiedByUrl = None;
        encoderDecoderLoc;
      })

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
        typeCreatorLocation = None;
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

let undefinedOrValueAsString ?(escape = false) v =
  match v with
  | None -> "?(None)"
  | Some v -> Printf.sprintf "\"%s\"" (if escape then Json.escape v else v)

let descriptionAsString v = undefinedOrValueAsString ~escape:true v

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

(** Pulls out name from `as` attribute. *)
let nameFromAttribute (attributes : Parsetree.attributes) ~default =
  match
    attributes
    |> List.find_map (fun (attr : Parsetree.attribute) ->
           match attr with
           | ( {Location.txt = "as"},
               PStr
                 [
                   {
                     pstr_desc =
                       Pstr_eval
                         ( {pexp_desc = Pexp_constant (Pconst_string (name, _))},
                           _ );
                   };
                 ] ) ->
             Some name
           | _ -> None)
  with
  | Some name -> name
  | None -> default

let findContextArgName (args : gqlArg list) =
  args
  |> List.find_map (fun (arg : gqlArg) ->
         match arg.typ with
         | InjectContext -> Some arg.name
         | _ -> None)

let findInterfaceTypeArgName (args : gqlArg list) =
  args
  |> List.find_map (fun (arg : gqlArg) ->
         match arg.typ with
         | InjectInterfaceTypename _ -> Some arg.name
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
         let fileUri =
           match t.typeCreatorLocation with
           | None -> t.typeLocation.fileUri |> Uri.toPath
           | Some {env} -> env.file.uri |> Uri.toPath
         in
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
               (String.concat "\n" (List.rev acc), lineNumber + 1, hasSpread)
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
             (String.concat "\n" (List.rev acc), lineNumber + 1, hasSpread)
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
                                           doesNotHaveField field.name)
                                    |> List.map (fun (field : gqlField) ->
                                           {
                                             field with
                                             onType = Some typ.displayName;
                                           }));
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

  (* Remove any interface that isn't actually implemented by any type or
     interface. Otherwise the codegen we do for interfaces will error out on a
     bunch of impossible states. *)

  (* TODO: Could instead tag this as "not implemented by anyone" if we want to
     provide a nice hover message to make the user understand why it's not
     included in codegen.
  *)
  schemaState.interfaces
  |> Hashtbl.iter (fun id (intf : gqlInterface) ->
         if Hashtbl.mem processedSchema.interfaceImplementedBy intf.id = false
         then Hashtbl.remove schemaState.interfaces id);

  GenerateSchemaValidation.validateSchema schemaState;
  processedSchema

let isPrintableArg (arg : gqlArg) =
  match arg.typ with
  | InjectContext | InjectInterfaceTypename _ -> false
  | _ -> true

(** Some arguments aren't intended to be printed in the `args` list, like
  `InjectContext` which controls injecting context into the resolver. *)
let onlyPrintableArgs (args : gqlArg list) = args |> List.filter isPrintableArg

let isFileContentsTheSame filePath s =
  try
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
  with Sys_error _ -> false

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

let writeIfHasChanges path contents =
  if isFileContentsTheSame path contents then ()
  else
    try
      let oc = open_out path in

      output_string oc contents;
      close_out oc
    with Sys_error _ ->
      Printf.printf
        "Something went wrong trying to write to \"%s\". Make sure the \
         directory actually exists."
        path;
      exit 1

let getStateFilePath (package : SharedTypes.package) =
  package.rootPath ^ "/lib/.resgraphState.marshal"

let stateFileExists (package : SharedTypes.package) =
  Files.exists (getStateFilePath package)

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

type scalarValidationResult = DoesNotNeedParsing | NeedsParsing
let rec validateCustomScalar ~env ~package (typ : Types.type_expr) =
  match typ.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    validateCustomScalar ~env ~package t1
  | Tconstr (Path.Pident {name = "option" | "array"}, [payloadTypeExpr], _) ->
    validateCustomScalar ~env ~package payloadTypeExpr
  | Tconstr (Path.Pident {name = "bool" | "string" | "int" | "float"}, [], _) ->
    DoesNotNeedParsing
  | Tconstr (path, typeArgs, _) -> (
    match pathIdentToList path with
    | ["JSON"; "t"]
    | ["Js"; "Json"; "t"]
    | ["Js"; "Nullable"; "t"]
    | ["Nullable"; "t"]
    | ["Js"; "Null"; "t"]
    | ["Null"; "t"] ->
      DoesNotNeedParsing
    | _ -> (
      match References.digConstructor ~env ~package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let t1 = t1 |> TypeUtils.instantiateType ~typeParams ~typeArgs in
        validateCustomScalar ~env ~package t1
      | _ -> NeedsParsing))
  | _ -> NeedsParsing

let emptyLoc =
  {
    Location.loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos;
    loc_ghost = true;
  }

let lastModuleInPath modulePath =
  let rec loop modulePath current =
    match modulePath with
    | SharedTypes.ModulePath.File _ -> current
    | IncludedModule (_, inner) -> loop inner current
    | ExportedModule {name; modulePath = inner} -> loop inner name
    | NotVisible -> current
  in
  loop modulePath ""
