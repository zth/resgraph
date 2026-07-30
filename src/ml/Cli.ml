let help =
  {|
**Private CLI For ResGraph**

Commands:
  generate-schema <projectRoot> <outputFolder> <printSdl:boolean> [options]
  completion <path> <line> <col> <currentFile>
  hover <path> <line> <col> [schema]
  hover-graphql <path> <hoverHint> [schema]
  definition-graphql <path> <definitionHint> [schema]
  find-definition <path> <definitionHint> [schema]
|}

let optional_arg = function
  | "-" -> None
  | value -> Some value

let optional_authorization_config =
  {
    GenerateSchemaTypes.mode = AuthorizationOptional;
    onForbidden = None;
    manifestPath = None;
    baselinePath = None;
  }

type generate_options = {
  schemaName: string option;
  moduleName: string;
  contextType: string;
  includePaths: string list;
  excludePaths: string list;
  authorizationConfig: GenerateSchemaTypes.authorizationConfig;
}

let default_generate_options =
  {
    schemaName = None;
    moduleName = "ResGraphSchema";
    contextType = "ResGraphContext.context";
    includePaths = [];
    excludePaths = [];
    authorizationConfig = optional_authorization_config;
  }

let parse_authorization_options options args =
  match args with
  | (("required" | "baseline") as mode)
    :: onForbidden :: manifestPath :: baselinePath :: rest
    when not (String.starts_with baselinePath ~prefix:"--") ->
    Some
      ( {
          options with
          authorizationConfig =
            {
              mode =
                (if mode = "baseline" then AuthorizationBaseline
                 else AuthorizationRequired);
              onForbidden = optional_arg onForbidden;
              manifestPath = optional_arg manifestPath;
              baselinePath = optional_arg baselinePath;
            };
        },
        rest )
  | "required" :: onForbidden :: manifestPath :: rest ->
    Some
      ( {
          options with
          authorizationConfig =
            {
              mode = AuthorizationRequired;
              onForbidden = optional_arg onForbidden;
              manifestPath = optional_arg manifestPath;
              baselinePath = None;
            };
        },
        rest )
  | ("required" | "baseline") :: _ -> None
  | _ -> Some (options, args)

let rec parse_generate_options options args =
  match args with
  | [] -> Some options
  | "--schema" :: schemaName :: rest ->
    parse_generate_options {options with schemaName = Some schemaName} rest
  | "--module" :: moduleName :: rest ->
    parse_generate_options {options with moduleName} rest
  | "--context" :: contextType :: rest ->
    parse_generate_options {options with contextType} rest
  | "--include" :: path :: rest ->
    parse_generate_options
      {options with includePaths = path :: options.includePaths}
      rest
  | "--exclude" :: path :: rest ->
    parse_generate_options
      {options with excludePaths = path :: options.excludePaths}
      rest
  | _ -> None

let matches pattern value = Str.string_match (Str.regexp pattern) value 0

let valid_generate_options options =
  (match options.schemaName with
    | None -> true
    | Some schemaName -> matches "^[A-Za-z0-9_-]+$" schemaName)
  && matches "^[A-Z][A-Za-z0-9_]*$" options.moduleName
  && matches "^[A-Z][A-Za-z0-9_]*\\(\\.[A-Za-z_][A-Za-z0-9_]*\\)+$"
       options.contextType

let run_generate ~sourceFolder ~outputFolder ~writeSdlFile options =
  GenerateSchemaDirect.generateSchemaDirect ~writeStateFile:true ~sourceFolder
    ~debug:false ~outputFolder ~writeSdlFile ~printToStdOut:true
    ~schemaName:options.schemaName ~moduleName:options.moduleName
    ~contextType:options.contextType
    ~includePaths:(List.rev options.includePaths)
    ~excludePaths:(List.rev options.excludePaths)
    ~authorizationConfig:options.authorizationConfig

let schema_name = function
  | [] -> Some None
  | [schemaName] -> Some (Some schemaName)
  | _ -> None

let main () =
  match Array.to_list Sys.argv with
  | _ :: "generate-schema" :: sourceFolder :: outputFolder :: writeSdl :: rest
    -> (
    match
      Option.bind (parse_authorization_options default_generate_options rest)
        (fun (options, rest) -> parse_generate_options options rest)
    with
    | Some options when valid_generate_options options ->
      run_generate ~sourceFolder ~outputFolder ~writeSdlFile:(writeSdl = "true")
        options
    | Some _ | None ->
      prerr_endline help;
      exit 1)
  | _ :: "completion" :: path :: line :: col :: currentFile :: rest -> (
    match schema_name rest with
    | Some schemaName ->
      Completion.completion ~debug:false ~path
        ~pos:(int_of_string line, int_of_string col)
        ~currentFile ~schemaName
    | None ->
      prerr_endline help;
      exit 1)
  | _ :: "hover" :: path :: line :: col :: rest -> (
    match schema_name rest with
    | Some _ ->
      Hover.hover ~path
        ~pos:(int_of_string line, int_of_string col)
        ~debug:false
    | None ->
      prerr_endline help;
      exit 1)
  | _ :: "hover-graphql" :: path :: hoverHint :: rest -> (
    match schema_name rest with
    | Some schemaName ->
      Hover.hoverGraphQL ~path ~hoverHint ~schemaName |> print_endline
    | None ->
      prerr_endline help;
      exit 1)
  | _ :: "definition-graphql" :: path :: definitionHint :: rest -> (
    match schema_name rest with
    | Some schemaName ->
      Hover.definitionGraphQL ~path ~definitionHint ~schemaName |> print_endline
    | None ->
      prerr_endline help;
      exit 1)
  | _ :: "find-definition" :: path :: definitionHint :: rest -> (
    match schema_name rest with
    | Some schemaName ->
      Analyze.findDefinition ~path ~definitionHint ~schemaName |> print_endline
    | None ->
      prerr_endline help;
      exit 1)
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
