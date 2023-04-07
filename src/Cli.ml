let help = {|
**Private CLI For ResGraph**
|}

let main () =
  match Array.to_list Sys.argv with
  | [_; "generate-schema"; path; schemaOutputPath; assetsOutputPath] ->
    GenerateSchema.generateSchema ~path ~debug:false ~schemaOutputPath
      ~assetsOutputPath
  | [_; "test"; path] ->
    Cfg.supportsSnippets := true;
    Commands.test ~path
  | args when List.mem "-h" args || List.mem "--help" args -> prerr_endline help
  | _ ->
    prerr_endline help;
    exit 1
;;

main ()
