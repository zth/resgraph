open Resgraph_engine
let help =
  {|
**Private CLI For ResGraph**

Commands:
  generate-schema <projectRoot> <outputFolder> <printSdl:boolean> [options]
  generate-schemas-v1 <length-prefixed generate-schema calls>
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

let run_generate ?generationContext ~sourceFolder ~outputFolder ~writeSdlFile
    options =
  GenerateSchemaDirect.generateSchemaDirect ?generationContext
    ~writeStateFile:true ~sourceFolder ~debug:false ~outputFolder ~writeSdlFile
    ~printToStdOut:true ~schemaName:options.schemaName
    ~moduleName:options.moduleName ~contextType:options.contextType
    ~includePaths:(List.rev options.includePaths)
    ~excludePaths:(List.rev options.excludePaths)
    ~authorizationConfig:options.authorizationConfig ()

let parse_generate_call = function
  | "generate-schema" :: sourceFolder :: outputFolder :: writeSdl :: rest -> (
    match
      Option.bind (parse_authorization_options default_generate_options rest)
        (fun (options, rest) -> parse_generate_options options rest)
    with
    | Some options when valid_generate_options options ->
      Some (sourceFolder, outputFolder, writeSdl = "true", options)
    | Some _ | None -> None)
  | _ -> None

let rec take_arguments count acc args =
  if count = 0 then Some (List.rev acc, args)
  else
    match args with
    | [] -> None
    | arg :: rest -> take_arguments (count - 1) (arg :: acc) rest

let rec parse_batch_calls acc = function
  | [] -> Some (List.rev acc)
  | length :: rest -> (
    match int_of_string_opt length with
    | Some length when length > 0 -> (
      match take_arguments length [] rest with
      | Some (call, remaining) -> (
        match parse_generate_call call with
        | Some parsed -> parse_batch_calls (parsed :: acc) remaining
        | None -> None)
      | None -> None)
    | Some _ | None -> None)

let run_batch calls =
  let generationContext = GenerationContext.create () in
  print_string "[";
  calls
  |> List.iteri
       (fun index (sourceFolder, outputFolder, writeSdlFile, options) ->
         if index > 0 then print_string ",";
         try
           run_generate ~generationContext ~sourceFolder ~outputFolder
             ~writeSdlFile options
         with exn ->
           Printf.printf "{\"status\":\"Error\",\"errors\":[%s]}"
             (GenerateSchemaUtils.printDiagnostic
                {
                  loc = Location.none;
                  fileUri = Uri.fromPath sourceFolder;
                  message =
                    "Schema generation failed: " ^ Printexc.to_string exn;
                }));
  print_string "]"

let schema_name = function
  | [] -> Some None
  | [schemaName] -> Some (Some schemaName)
  | _ -> None

let main () =
  match Array.to_list Sys.argv with
  | _ :: "generate-schema" :: rest -> (
    match parse_generate_call ("generate-schema" :: rest) with
    | Some (sourceFolder, outputFolder, writeSdlFile, options) ->
      run_generate ~sourceFolder ~outputFolder ~writeSdlFile options
    | None ->
      prerr_endline help;
      exit 1)
  | _ :: "generate-schemas-v1" :: rest -> (
    match parse_batch_calls [] rest with
    | Some calls when calls <> [] -> run_batch calls
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

try main () with Sys_error _ | Unix.Unix_error _ -> exit 1
