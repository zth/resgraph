open Resgraph_engine
open GenerateSchemaTypes

let fail message =
  prerr_endline message;
  exit 1

let assertTrue condition message = if not condition then fail message

let makeSchemaState () : schemaState =
  {
    contextTypePath = ["ResGraphContext"; "context"];
    rootFileUri = Uri.fromPath "/tmp/resgraph-architecture-test";
    types = Hashtbl.create 1;
    inputObjects = Hashtbl.create 1;
    inputUnions = Hashtbl.create 1;
    enums = Hashtbl.create 1;
    unions = Hashtbl.create 1;
    interfaces = Hashtbl.create 1;
    scalars = Hashtbl.create 1;
    processedFiles = Hashtbl.create 1;
    authorizationConfig =
      {
        mode = AuthorizationOptional;
        onForbidden = None;
        manifestPath = None;
        baselinePath = None;
      };
    authorizationDeclarations = Hashtbl.create 1;
    authorizationPlans = Hashtbl.create 1;
    resolverOutcomes = Hashtbl.create 1;
    authorizationExemptions = Hashtbl.create 1;
    authorizationGaps = [];
    query = None;
    subscription = None;
    mutation = None;
    diagnostics = [];
  }

let makePackage rootPath : SharedTypes.package =
  {
    genericJsxModule = None;
    suffix = ".mjs";
    rootPath;
    projectFiles = SharedTypes.FileSet.empty;
    dependenciesFiles = SharedTypes.FileSet.empty;
    pathsForModule = Hashtbl.create 1;
    namespace = None;
    opens = [];
    uncurried = true;
    rescriptVersion = (12, 0);
    autocomplete = Misc.StringMap.empty;
  }

let testGenerationContextScopesSummariesByPackage () =
  let context = GenerationContext.create () in
  let firstPackage = makePackage "/tmp/resgraph-package-a" in
  let secondPackage = makePackage "/tmp/resgraph-package-b" in
  let moduleName = "Shared" in
  let firstFile =
    SharedTypes.File.create moduleName
      (Uri.fromPath "/tmp/resgraph-package-a/src/Shared.res")
  in
  let secondFile =
    SharedTypes.File.create moduleName
      (Uri.fromPath "/tmp/resgraph-package-b/src/Shared.res")
  in
  GenerationContext.seedSummary context ~package:firstPackage ~moduleName
    firstFile;
  GenerationContext.seedSummary context ~package:secondPackage ~moduleName
    secondFile;
  let loadedPath package =
    match GenerationContext.loadSummary context ~package ~moduleName with
    | Some file -> Uri.toPath file.uri
    | None -> fail "A seeded generation summary must be available."
  in
  assertTrue
    (loadedPath firstPackage = "/tmp/resgraph-package-a/src/Shared.res")
    "A generation context must not reuse a same-named module from another \
     package.";
  assertTrue
    (loadedPath secondPackage = "/tmp/resgraph-package-b/src/Shared.res")
    "A generation context must retain summaries independently per package."

let testInputUnionRegistry () =
  let schemaState = makeSchemaState () in
  let fileUri = Uri.fromPath "/tmp/Schema.res" in
  Hashtbl.add schemaState.unions "shared"
    {
      typeSource = Variant;
      id = "shared";
      displayName = "OutputChoice";
      description = None;
      types = [];
      typeLocation = Synthetic {fileName = "Schema"; fileUri; modulePath = []};
    };
  GenerateSchemaUtils.addInputUnion "shared" ~debug:false ~schemaState
    ~makeInputUnion:(fun () ->
      {
        id = "shared";
        displayName = "InputChoice";
        members = [];
        description = None;
        typeLocation =
          {
            fileName = "Schema";
            fileUri;
            modulePath = [];
            typeName = "shared";
            loc = Location.none;
          };
      });
  assertTrue
    (Hashtbl.mem schemaState.inputUnions "shared")
    "A regular union must not suppress an input union with the same internal \
     id."

let testAtomicWrite () =
  let path = Filename.temp_file "resgraph-atomic-write-" ".txt" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      GenerateSchemaUtils.writeAtomically path "first";
      GenerateSchemaUtils.writeIfHasChanges path "second";
      match Files.readFile path with
      | Some "second" -> ()
      | Some contents ->
        fail ("Atomic write returned unexpected contents: " ^ contents)
      | None -> fail "Atomic write did not leave the target file.")

let testWriteFailureIsRaised () =
  let missingParent = Filename.temp_file "resgraph-missing-parent-" "" in
  Sys.remove missingParent;
  let path = Filename.concat missingParent "artifact.res" in
  match GenerateSchemaUtils.writeIfHasChanges path "contents" with
  | () ->
    fail "A generated artifact write failure must be raised to the caller."
  | exception (Sys_error _ | Unix.Unix_error _) -> ()
  | exception exn -> raise exn

let testCrossKindGraphqlNameCollision () =
  let schemaState = makeSchemaState () in
  let fileUri = Uri.fromPath "/tmp/Schema.res" in
  let typeLocation =
    {
      fileName = "Schema";
      fileUri;
      modulePath = [];
      typeName = "choice";
      loc = Location.none;
    }
  in
  Hashtbl.add schemaState.enums "choice"
    {
      id = "choice";
      displayName = "Choice";
      values = [];
      description = None;
      typeLocation = Concrete typeLocation;
    };
  Hashtbl.add schemaState.inputObjects "choiceInput"
    {
      id = "choiceInput";
      displayName = "Choice";
      fields = [];
      description = None;
      typeLocation = Some typeLocation;
      syntheticTypeLocation = None;
    };
  GenerateSchemaValidation.validateTypeNameUniqueness schemaState;
  assertTrue
    (List.length schemaState.diagnostics = 1)
    "Cross-kind GraphQL type names must be rejected before emission."

let () =
  testGenerationContextScopesSummariesByPackage ();
  testInputUnionRegistry ();
  testAtomicWrite ();
  testWriteFailureIsRaised ();
  testCrossKindGraphqlNameCollision ();
  print_endline "Native architecture fixtures passed."
