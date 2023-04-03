let help =
  {|
**Private CLI For rescript-vscode usage only**

API examples:
  ./resgraph.exe completion src/MyFile.res 0 4 currentContent.res true
  ./resgraph.exe definition src/MyFile.res 9 3
  ./resgraph.exe typeDefinition src/MyFile.res 9 3
  ./resgraph.exe documentSymbol src/Foo.res
  ./resgraph.exe hover src/MyFile.res 10 2 true
  ./resgraph.exe references src/MyFile.res 10 2
  ./resgraph.exe rename src/MyFile.res 10 2 foo
  ./resgraph.exe diagnosticSyntax src/MyFile.res
  ./resgraph.exe inlayHint src/MyFile.res 0 3 25
  ./resgraph.exe codeLens src/MyFile.res

Dev-time examples:
  ./resgraph.exe dump src/MyFile.res src/MyFile2.res
  ./resgraph.exe test src/MyFile.res

Note: positions are zero-indexed (start at 0 0), following LSP.
https://microsoft.github.io/language-server-protocol/specification#position

Options:
  completion: compute autocomplete for MyFile.res at line 0 and column 4,
    where MyFile.res is being edited and the editor content is in file current.res.

    ./resgraph.exe completion src/MyFile.res 0 4 current.res

  definition: get definition for item in MyFile.res at line 10 column 2:

    ./resgraph.exe definition src/MyFile.res 10 2

  typeDefinition: get type definition for item in MyFile.res at line 10 column 2:

    ./resgraph.exe typeDefinition src/MyFile.res 10 2

  documentSymbol: get all symbols declared in MyFile.res

    ./resgraph.exe documentSymbol src/MyFile.res

  hover: get inferred type for MyFile.res at line 10 column 2 (supporting markdown links):

    ./resgraph.exe hover src/MyFile.res 10 2 true

  references: get all references to item in MyFile.res at line 10 column 2:

    ./resgraph.exe references src/MyFile.res 10 2

  rename: rename all appearances of item in MyFile.res at line 10 column 2 with foo:

    ./resgraph.exe rename src/MyFile.res 10 2 foo

  semanticTokens: return token semantic highlighting info for MyFile.res

    ./resgraph.exe semanticTokens src/MyFile.res

  createInterface: print to stdout the interface file for src/MyFile.res

    ./resgraph.exe createInterface src/MyFile.res lib/bs/src/MyFile.cmi

  format: print to stdout the formatted version of the provided file

    ./resgraph.exe format src/MyFile.res

  diagnosticSyntax: print to stdout diagnostic for syntax

    ./resgraph.exe diagnosticSyntax src/MyFile.res

  inlayHint: get all inlay Hint between line 0 and 3 declared in MyFile.res. Last argument is maximum of character length for inlay hints

    ./resgraph.exe inlayHint src/MyFile.res 0 3 25

  codeLens: get all code lens entries for file src/MyFile.res

    ./resgraph.exe codeLens src/MyFile.res

  signatureHelp: get signature help if available for position at line 10 column 2 in src/MyFile.res

    ./resgraph.exe signatureHelp src/MyFile.res 10 2

  test: run tests specified by special comments in file src/MyFile.res

    ./resgraph.exe test src/src/MyFile.res
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
