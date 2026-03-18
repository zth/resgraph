open GenerateSchemaTypes

type hoverGqlType =
  | ObjectType of gqlObjectType
  | Interface of gqlInterface
  | Enum of gqlEnum
  | InputObject of gqlInputObjectType
  | InputUnion of gqlInputUnionType
  | Union of gqlUnion
  | Scalar of gqlScalar

let findGqlTypeExact typename ~schemaState =
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
          match Hashtbl.find_opt schemaState.inputUnions typename with
          | Some union -> Some (InputUnion union)
          | None -> (
            match Hashtbl.find_opt schemaState.unions typename with
            | Some union -> Some (Union union)
            | None -> (
              match Hashtbl.find_opt schemaState.scalars typename with
              | Some scalar -> Some (Scalar scalar)
              | None -> None))))))

let findGqlType typename ~schemaState =
  let candidateNames =
    [
      typename;
      GenerateSchemaUtils.uncapitalizeFirstChar typename;
      GenerateSchemaUtils.capitalizeFirstChar typename;
    ]
  in
  candidateNames
  |> List.find_map (fun candidate -> findGqlTypeExact candidate ~schemaState)

let makeTypeHoverText ~typename ~(typeLocation : typeLocationLoc) =
  Printf.sprintf "%s type defined by ResGraph.\n" typename
  ^ Markdown.divider
  ^ Markdown.goToDefinitionText ~loc:typeLocation.loc
      ~fileUri:typeLocation.fileUri

let hover ~path:_ ~pos:_ ~debug:_ =
  Printf.printf "{\"status\": \"Hover\", \"item\": %s}" Protocol.null

let hoverGraphQL ~path ~hoverHint =
  match Packages.getPackage ~uri:(Uri.fromPath path) with
  | None -> Protocol.null
  | Some package ->
    let schemaState, _ = GenerateSchemaUtils.readStateFile ~package in
    let hoverStr =
      match hoverHint |> String.split_on_char '.' with
      | [typename] -> (
        match findGqlType typename ~schemaState with
        | None -> Protocol.null
        | Some (Scalar typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation ~typename:"Scalar")
        | Some (ObjectType {syntheticTypeLocation = Some {fileUri; loc}}) ->
          Protocol.stringifyHover
            (Printf.sprintf "%s type defined by an inline record.\n" typename
            ^ Markdown.divider
            ^ Markdown.goToDefinitionText ~loc ~fileUri)
        | Some (ObjectType {typeLocation = Some (Concrete typeLocation)}) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation ~typename:"Object")
        | Some (Interface typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation
               ~typename:"Interface")
        | Some (Enum {typeLocation = Concrete typeLocation}) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation ~typename:"Enum")
        | Some (InputObject {typeLocation = Some typeLocation}) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation ~typename:"Input object")
        | Some (Union {typeLocation = Concrete typeLocation}) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation ~typename:"Union")
        | Some (InputUnion typ) ->
          Protocol.stringifyHover
            (makeTypeHoverText ~typeLocation:typ.typeLocation
               ~typename:"Input union")
        | Some (ObjectType _) | Some (Union _) | Some (Enum _) -> Protocol.null
        | Some (InputObject {typeLocation = None}) -> Protocol.null)
      | [typename; fieldName] -> (
        match findGqlType typename ~schemaState with
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
        match findGqlType typename ~schemaState with
        | Some (Scalar {typeLocation = {fileUri; loc}})
        | Some (ObjectType {syntheticTypeLocation = Some {fileUri; loc}})
        | Some (ObjectType {typeLocation = Some (Concrete {fileUri; loc})})
        | Some (Interface {typeLocation = {fileUri; loc}})
        | Some (Enum {typeLocation = Concrete {fileUri; loc}})
        | Some (InputObject {syntheticTypeLocation = Some {fileUri; loc}})
        | Some (InputObject {typeLocation = Some {fileUri; loc}})
        | Some
            ( Union {typeLocation = Concrete {fileUri; loc}}
            | InputUnion {typeLocation = {fileUri; loc}} ) ->
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
        match findGqlType typename ~schemaState with
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
