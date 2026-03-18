open GenerateSchemaTypes

type position = {line: int; column: int}
type range = {start: position; end_: position}

type definition = {path: string; kind: string; file: string; range: range}

let wrap_in_quotes = Protocol.wrapInQuotes

let stringify_position {line; column} =
  Printf.sprintf {|{"line":%d,"column":%d}|} line column

let stringify_range {start; end_} =
  Printf.sprintf {|{"start":%s,"end":%s}|} (stringify_position start)
    (stringify_position end_)

let stringify_definition {path; kind; file; range} =
  Protocol.stringifyObject
    [
      ("path", Some (wrap_in_quotes path));
      ("kind", Some (wrap_in_quotes kind));
      ("file", Some (wrap_in_quotes file));
      ("range", Some (stringify_range range));
    ]

let stringify_response ?item ?error () =
  Protocol.stringifyObject
    [
      ("status", Some (wrap_in_quotes "FindDefinition"));
      ("item", item |> Option.map stringify_definition);
      ("error", error |> Option.map wrap_in_quotes);
    ]

let position_of_lexing pos =
  let line, column = Pos.ofLexing pos in
  {line = line + 1; column = column + 1}

let range_of_loc (loc : Location.t) =
  {
    start = position_of_lexing loc.loc_start;
    end_ = position_of_lexing loc.loc_end;
  }

let make_definition ~path ~kind ~fileUri ~loc =
  {path; kind; file = fileUri |> Uri.toPath; range = range_of_loc loc}

let load_schema_state ~path =
  match Packages.getPackage ~uri:(Uri.fromPath path) with
  | None ->
    Error (Printf.sprintf "Path \"%s\" is not inside a ReScript project." path)
  | Some package -> (
    if not (GenerateSchemaUtils.stateFileExists package) then
      Error
        (Printf.sprintf
           "No ResGraph state file was found for this project. Run `resgraph \
            build` first.")
    else
      try Ok (GenerateSchemaUtils.readStateFile ~package |> fst)
      with _ ->
        Error
          "ResGraph could not read the generated schema state. Run `resgraph \
           build` again.")

let resolve_type_definition ~schemaState ~definitionHint typename =
  match
    Hover.findGqlType
      (typename |> GenerateSchemaUtils.uncapitalizeFirstChar)
      ~schemaState
  with
  | Some (Hover.Scalar {typeLocation = {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"scalar" ~fileUri ~loc)
  | Some (Hover.ObjectType {syntheticTypeLocation = Some {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"objectType" ~fileUri ~loc)
  | Some (Hover.ObjectType {typeLocation = Some (Concrete {fileUri; loc})}) ->
    Ok (make_definition ~path:definitionHint ~kind:"objectType" ~fileUri ~loc)
  | Some (Hover.Interface {typeLocation = {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"interface" ~fileUri ~loc)
  | Some (Hover.Enum {typeLocation = Concrete {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"enum" ~fileUri ~loc)
  | Some (Hover.InputObject {syntheticTypeLocation = Some {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"inputObject" ~fileUri ~loc)
  | Some (Hover.InputObject {typeLocation = Some {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"inputObject" ~fileUri ~loc)
  | Some (Hover.Union {typeLocation = Concrete {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"union" ~fileUri ~loc)
  | Some (Hover.InputUnion {typeLocation = {fileUri; loc}}) ->
    Ok (make_definition ~path:definitionHint ~kind:"inputUnion" ~fileUri ~loc)
  | Some _ ->
    Error
      (Printf.sprintf
         "GraphQL type `%s` exists, but ResGraph could not determine a source \
          definition for it."
         typename)
  | None -> Error (Printf.sprintf "Could not find GraphQL type `%s`." typename)

let resolve_field_definition ~schemaState ~definitionHint typename fieldName =
  match
    Hover.findGqlType
      (typename |> GenerateSchemaUtils.uncapitalizeFirstChar)
      ~schemaState
  with
  | Some
      ( Hover.ObjectType {fields}
      | Hover.Interface {fields}
      | Hover.InputObject {fields} ) -> (
    match
      fields |> List.find_opt (fun (field : gqlField) -> field.name = fieldName)
    with
    | None ->
      Error
        (Printf.sprintf "Could not find field `%s` on GraphQL type `%s`."
           fieldName typename)
    | Some {resolverStyle = Resolver _; loc; fileUri} ->
      Ok (make_definition ~path:definitionHint ~kind:"resolver" ~fileUri ~loc)
    | Some {resolverStyle = Property _; loc; fileUri} ->
      Ok
        (make_definition ~path:definitionHint ~kind:"exposedField" ~fileUri ~loc)
    )
  | Some _ ->
    Error (Printf.sprintf "GraphQL type `%s` does not expose fields." typename)
  | None -> Error (Printf.sprintf "Could not find GraphQL type `%s`." typename)

let findDefinition ~path ~definitionHint =
  try
    match load_schema_state ~path with
    | Error error -> stringify_response ~error ()
    | Ok schemaState -> (
      match definitionHint |> String.split_on_char '.' with
      | [typename] -> (
        match resolve_type_definition ~schemaState ~definitionHint typename with
        | Ok item -> stringify_response ~item ()
        | Error error -> stringify_response ~error ())
      | [typename; fieldName] -> (
        match
          resolve_field_definition ~schemaState ~definitionHint typename
            fieldName
        with
        | Ok item -> stringify_response ~item ()
        | Error error -> stringify_response ~error ())
      | _ ->
        stringify_response
          ~error:
            "Invalid definition path. Use `TypeName` or `TypeName.fieldName`."
          ())
  with _ ->
    stringify_response ~error:"ResGraph failed to analyze the definition." ()
