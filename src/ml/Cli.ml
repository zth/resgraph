let help = {|
**Private CLI For ResGraph**

Commands:
  generate-schema <sourceFolder> <outputFolder> [printSdl:boolean]
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
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
