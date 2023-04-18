let help = {|
**Private CLI For ResGraph**
|}

let main () =
  match Array.to_list Sys.argv with
  | [_; "generate-schema"; sourceFolder; outputFolder] ->
    GenerateSchema.generateSchema ~writeStateFile:true ~sourceFolder
      ~debug:false ~outputFolder
  | [_; "completion"; path; line; col; currentFile] ->
    Completion.completion ~debug:false ~path
      ~pos:(int_of_string line, int_of_string col)
      ~currentFile
  | [_; "test"; path] ->
    Cfg.supportsSnippets := true;
    Commands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
