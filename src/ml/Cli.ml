let help =
  {|
**Private CLI For ResGraph**

Commands:
  generate-schema <sourceFolder> <outputFolder> [printSdl:boolean]
  authorization-bootstrap <sourceFolder> <outputFolder>
  completion <path> <line> <col> <currentFile>
  hover <path> <line> <col>
  hover-graphql <path> <hoverHint>
  definition-graphql <path> <definitionHint>
  find-definition <path> <definitionHint>
|}

let optional_arg = function
  | "-" -> None
  | value -> Some value

let optional_authorization_config =
  {
    GenerateSchemaTypes.mode = AuthorizationOptional;
    onForbidden = None;
    manifestPath = None;
  }

let run_generate ~sourceFolder ~outputFolder ~writeSdlFile ~authorizationConfig
    =
  GenerateSchemaDirect.generateSchemaDirect ~writeStateFile:true ~sourceFolder
    ~debug:false ~outputFolder ~writeSdlFile ~printToStdOut:true
    ~authorizationConfig

let run_authorization_bootstrap ~sourceFolder ~outputFolder =
  GenerateSchemaDirect.generateSchemaDirect ~writeStateFile:false ~sourceFolder
    ~debug:false ~outputFolder ~writeSdlFile:false ~printToStdOut:true
    ~authorizationConfig:
      {mode = AuthorizationRequired; onForbidden = None; manifestPath = None}

let main () =
  match Array.to_list Sys.argv with
  | [_; "authorization-bootstrap"; sourceFolder; outputFolder] ->
    run_authorization_bootstrap ~sourceFolder ~outputFolder
  | [
   _;
   "generate-schema";
   sourceFolder;
   outputFolder;
   (("true" | "false") as writeSdlFile);
   "required";
   onForbidden;
   manifestPath;
  ] ->
    run_generate ~sourceFolder ~outputFolder
      ~writeSdlFile:(writeSdlFile = "true")
      ~authorizationConfig:
        {
          mode = AuthorizationRequired;
          onForbidden = optional_arg onForbidden;
          manifestPath = optional_arg manifestPath;
        }
  | [_; "generate-schema"; sourceFolder; outputFolder; "true"] ->
    run_generate ~sourceFolder ~outputFolder ~writeSdlFile:true
      ~authorizationConfig:optional_authorization_config
  | [_; "generate-schema"; sourceFolder; outputFolder; "false"]
  | [_; "generate-schema"; sourceFolder; outputFolder] ->
    run_generate ~sourceFolder ~outputFolder ~writeSdlFile:false
      ~authorizationConfig:optional_authorization_config
  | [_; "completion"; path; line; col; currentFile] ->
    Completion.completion ~debug:false ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile
  | [_; "hover"; path; line; col] ->
    Hover.hover ~path ~pos:(int_of_string line, int_of_string col) ~debug:false
  | [_; "hover-graphql"; path; hoverHint] ->
    Hover.hoverGraphQL ~path ~hoverHint |> print_endline
  | [_; "definition-graphql"; path; definitionHint] ->
    Hover.definitionGraphQL ~path ~definitionHint |> print_endline
  | [_; "find-definition"; path; definitionHint] ->
    Analyze.findDefinition ~path ~definitionHint |> print_endline
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
