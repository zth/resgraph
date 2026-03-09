let help =
  {|
**Private CLI For ResGraph**

Commands:
  generate-schema <sourceFolder> <outputFolder> [printSdl:boolean]
  completion <path> <line> <col> <currentFile>
  hover <path> <line> <col>
  hover-graphql <path> <hoverHint>
  definition-graphql <path> <definitionHint>
|}

let run_generate ~sourceFolder ~outputFolder ~writeSdlFile =
  GenerateSchemaDirect.generateSchemaDirect ~writeStateFile:true ~sourceFolder
    ~debug:false ~outputFolder ~writeSdlFile ~printToStdOut:true

let main () =
  match Array.to_list Sys.argv with
  | [_; "generate-schema"; sourceFolder; outputFolder; "true"] ->
    run_generate ~sourceFolder ~outputFolder ~writeSdlFile:true
  | [_; "generate-schema"; sourceFolder; outputFolder] ->
    run_generate ~sourceFolder ~outputFolder ~writeSdlFile:false
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
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
