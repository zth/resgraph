open SharedTypes
open GenerateSchemaTypes

let banner typ = Printf.sprintf "GraphQL %s generated by ResGraph:\n" typ

let newHover ~full locItem =
  let {file; package} = full in
  let env = QueryEnv.fromFile file in
  let schemaState =
    Lazy.from_fun (fun () -> GenerateSchemaUtils.readStateFile ~package |> fst)
  in

  match locItem.locType with
  | TypeDefinition (name, decl, stamp) -> (
    let schemaState = Lazy.force schemaState in
    match
      GenerateSchemaUtils.extractGqlAttribute ~schemaState
        ~env:(QueryEnv.fromFile file) decl.type_attributes
    with
    | Some InputObject -> (
      match Hashtbl.find_opt schemaState.inputObjects name with
      | None -> None
      | Some input ->
        Some
          (banner "input object"
          ^ Markdown.graphqlCodeBlock (GenerateSchemaSDL.printInputObject input)
          ))
    | Some ObjectType -> (
      match Hashtbl.find_opt schemaState.types name with
      | None -> None
      | Some typ ->
        Some
          (banner "type"
          ^ Markdown.graphqlCodeBlock (GenerateSchemaSDL.printObjectType typ)))
    | Some Enum -> (
      match Hashtbl.find_opt schemaState.enums name with
      | None -> None
      | Some enum ->
        Some
          (banner "enum"
          ^ Markdown.graphqlCodeBlock (GenerateSchemaSDL.printEnum enum)))
    | Some Union -> (
      match Hashtbl.find_opt schemaState.unions name with
      | None -> None
      | Some union ->
        Some
          (banner "union"
          ^ Markdown.graphqlCodeBlock (GenerateSchemaSDL.printUnion union)))
    | Some Scalar when name = "t" -> (
      (* module Timestamp = { @gql.scalar type = float } *)
      (* The module name is the scalar name here. *)
      match Stamps.findType file.stamps stamp with
      | None -> None
      | Some declared -> (
        let moduleName =
          GenerateSchemaUtils.lastModuleInPath declared.modulePath
        in
        match Hashtbl.find_opt schemaState.scalars moduleName with
        | None -> None
        | Some scalar ->
          Some
            (banner "scalar"
            ^ Markdown.graphqlCodeBlock (GenerateSchemaSDL.printScalar scalar)))
      )
    | Some Scalar -> (
      match Hashtbl.find_opt schemaState.scalars name with
      | None -> None
      | Some scalar ->
        Some
          (banner "scalar"
          ^ Markdown.graphqlCodeBlock (GenerateSchemaSDL.printScalar scalar)))
    | Some Interface -> (
      match Hashtbl.find_opt schemaState.interfaces name with
      | None -> None
      | Some intf ->
        Some
          (banner "interface"
          ^ Markdown.graphqlCodeBlock (GenerateSchemaSDL.printInterface intf)))
    | Some (InterfaceResolver _) | Some Field | None -> None)
  | Typed (_, _t, locKind) -> (
    match References.definedForLoc2 ~file ~package locKind with
    | None -> None
    | Some d -> (
      match Stamps.findValue file.stamps d.stamp with
      | None -> None
      | Some declared -> (
        let schemaState = Lazy.force schemaState in
        match
          GenerateSchema.extractResolverFunctionInfo ~resolverName:d.name.txt
            ~full ~env ~schemaState ~debug:false declared.item
        with
        | Some (GraphQLObjectType {id}, _, _) -> (
          let objectType = Hashtbl.find schemaState.types id in
          match
            objectType.fields
            |> List.find_opt (fun (f : gqlField) -> f.name = declared.name.txt)
          with
          | None -> None
          | Some f ->
            Some
              (banner "object type field"
              ^ Markdown.graphqlCodeBlock
                  (Printf.sprintf "type %s {\n%s\n}" objectType.displayName
                     (GenerateSchemaSDL.printFields [f]))))
        | Some (GraphQLInterface {id}, _, _) -> (
          let intf = Hashtbl.find schemaState.interfaces id in
          match
            intf.fields
            |> List.find_opt (fun (f : gqlField) -> f.name = declared.name.txt)
          with
          | None -> None
          | Some f ->
            Some
              (banner "interface field"
              ^ Markdown.graphqlCodeBlock
                  (Printf.sprintf "interface %s {\n%s\n}" intf.displayName
                     (GenerateSchemaSDL.printFields [f]))))
        | _ -> None)))
  | _ -> None

let hover ~path ~pos ~debug =
  let result =
    match Cmt.loadFullCmtFromPath ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None ->
        if debug then Printf.printf "Nothing at that position.\n";
        Protocol.null
      | Some locItem -> (
        let hoverText = newHover ~full locItem in
        match hoverText with
        | None -> Protocol.null
        | Some s -> Protocol.stringifyHover s))
  in
  Printf.printf "{\"status\": \"Hover\", \"item\": %s}" result

type hoverGqlType =
  | ObjectType of gqlObjectType
  | Interface of gqlInterface
  | Enum of gqlEnum
  | InputObject of gqlInputObjectType
  | Union of gqlUnion
  | Scalar of gqlScalar

let findGqlType typename ~schemaState =
  match Hashtbl.find_opt schemaState.types typename with
  | Some obj -> Some (ObjectType obj)
  | None -> (
    match Hashtbl.find_opt schemaState.interfaces typename with
    | Some intf -> Some (Interface intf)
    | None -> (
      match Hashtbl.find_opt schemaState.enums typename with
      | Some enum -> Some (Enum enum)
      | None -> (
        match Hashtbl.find_opt schemaState.inputObjects typename with
        | Some inputobj -> Some (InputObject inputobj)
        | None -> (
          match Hashtbl.find_opt schemaState.unions typename with
          | Some union -> Some (Union union)
          | None -> (
            match Hashtbl.find_opt schemaState.scalars typename with
            | Some scalar -> Some (Scalar scalar)
            | None -> (
              match
                Hashtbl.find_opt schemaState.types
                  (* This is very hacky. Fix in refactor fixing IDs *)
                  (GenerateSchemaUtils.capitalizeFirstChar typename)
              with
              | Some obj -> Some (ObjectType obj)
              | None -> None))))))

let makeTypeHoverText ~typename ~(typeLocation : typeLocation) =
  Printf.sprintf "%s type defined by ResGraph.\n" typename
  ^ Markdown.divider
  ^ Markdown.goToDefinitionText ~loc:typeLocation.loc
      ~fileUri:typeLocation.fileUri

let hoverGraphQL ~path ~hoverHint =
  match Packages.getPackage ~uri:(Uri.fromPath path) with
  | None -> Protocol.null
  | Some package ->
    let schemaState, _ = GenerateSchemaUtils.readStateFile ~package in
    let hoverStr =
      match hoverHint |> String.split_on_char '.' with
      | [typename] -> (
        match
          findGqlType
            (typename |> GenerateSchemaUtils.uncapitalizeFirstChar)
            ~schemaState
        with
        | None -> Protocol.null
        | Some (Scalar typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation ~typename:"Scalar")
        | Some (ObjectType {syntheticTypeLocation = Some {fileUri; loc}}) ->
          Protocol.stringifyHover
            (Printf.sprintf "%s type defined by an inline record.\n" typename
            ^ Markdown.divider
            ^ Markdown.goToDefinitionText ~loc ~fileUri)
        | Some (ObjectType {typeLocation = Some typeLocation}) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation ~typename:"Object")
        | Some (Interface typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation
               ~typename:"Interface")
        | Some (Enum typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation ~typename:"Enum")
        | Some (InputObject typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation
               ~typename:"Input object")
        | Some (Union typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation ~typename:"Union")
        | Some (ObjectType {typeLocation = None}) -> Protocol.null)
      | [typename; fieldName] -> (
        match
          findGqlType
            (typename |> GenerateSchemaUtils.uncapitalizeFirstChar)
            ~schemaState
        with
        | Some (ObjectType {fields} | Interface {fields} | InputObject {fields})
          -> (
          match
            fields |> List.find_opt (fun (f : gqlField) -> f.name = fieldName)
          with
          | None -> Protocol.null
          | Some {name; resolverStyle = Property propName; loc; fileUri} ->
            Protocol.stringifyHover
              (Printf.sprintf
                 "Field `%s` is a regular field coming directly from the \
                  property `%s`.\n"
                 name propName
              ^ Markdown.divider
              ^ Markdown.goToDefinitionText ~loc ~fileUri)
          | Some {name; resolverStyle = Resolver _resolver; loc; fileUri} ->
            Protocol.stringifyHover
              (Printf.sprintf "Field `%s` is a field function.\n" name
              ^ Markdown.divider
              ^ Markdown.goToDefinitionText ~loc ~fileUri))
        | _ -> Protocol.null)
      | _ -> Protocol.null
    in
    Printf.sprintf "{\"status\": \"Hover\", \"item\": %s}" hoverStr

let definitionGraphQL ~path ~definitionHint =
  match Packages.getPackage ~uri:(Uri.fromPath path) with
  | None -> Protocol.null
  | Some package ->
    let schemaState, _ = GenerateSchemaUtils.readStateFile ~package in
    let definitionLoc =
      match definitionHint |> String.split_on_char '.' with
      | [typename] -> (
        match
          findGqlType
            (typename |> GenerateSchemaUtils.uncapitalizeFirstChar)
            ~schemaState
        with
        | Some (Scalar {typeLocation = {fileUri; loc}})
        | Some (ObjectType {syntheticTypeLocation = Some {fileUri; loc}})
        | Some (ObjectType {typeLocation = Some {fileUri; loc}})
        | Some (Interface {typeLocation = {fileUri; loc}})
        | Some (Enum {typeLocation = {fileUri; loc}})
        | Some (InputObject {typeLocation = {fileUri; loc}})
        | Some (Union {typeLocation = {fileUri; loc}}) ->
          Some
            {
              Protocol.uri = fileUri |> Uri.toString;
              range =
                {
                  start =
                    (let line, character = loc.loc_start |> Pos.ofLexing in
                     {line; character});
                  end_ =
                    (let line, character = loc.loc_end |> Pos.ofLexing in
                     {line; character});
                };
            }
        | _ -> None)
      | [typename; fieldName] -> (
        match
          findGqlType
            (typename |> GenerateSchemaUtils.uncapitalizeFirstChar)
            ~schemaState
        with
        | Some (ObjectType {fields} | Interface {fields} | InputObject {fields})
          -> (
          match
            fields |> List.find_opt (fun (f : gqlField) -> f.name = fieldName)
          with
          | None -> None
          | Some {loc; fileUri} ->
            Some
              {
                Protocol.uri = fileUri |> Uri.toString;
                range =
                  {
                    start =
                      (let line, character = loc.loc_start |> Pos.ofLexing in
                       {line; character});
                    end_ =
                      (let line, character = loc.loc_end |> Pos.ofLexing in
                       {line; character});
                  };
              })
        | _ -> None)
      | _ -> None
    in
    Printf.sprintf "{\"status\": \"Definition\", \"item\": %s}"
      (match definitionLoc with
      | None -> Protocol.null
      | Some location -> Protocol.stringifyLocation location)
