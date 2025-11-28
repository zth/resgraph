let help = {|
**Private CLI For ResGraph**

Commands:
  generate-schema <sourceFolder> <outputFolder> [printSdl:boolean]
  test <file.res>
|}

let main () =
  match Array.to_list Sys.argv with
  | [_; "generate-schema"; sourceFolder; outputFolder; "true"] ->
    GenerateSchema.generateSchema ~writeStateFile:true ~sourceFolder
      ~debug:false ~outputFolder ~writeSdlFile:true ~printToStdOut:true
  | [_; "generate-schema"; sourceFolder; outputFolder] ->
    GenerateSchema.generateSchema ~writeStateFile:true ~sourceFolder
      ~debug:false ~outputFolder ~writeSdlFile:false ~printToStdOut:true
  | [_; "test"; path] -> Commands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
