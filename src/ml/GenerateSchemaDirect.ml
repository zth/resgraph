open SharedTypes
open GenerateSchemaTypes

type loaded = {
  moduleName: string;
  sourcePath: string;
  cmtPath: string;
  cmt: CmtDirect.t;
}

type collect_error = {file: string; message: string}

let load_cmt ~package ~moduleName ~sourcePath =
  match Hashtbl.find_opt package.pathsForModule moduleName with
  | None ->
    Error
      {
        file = sourcePath;
        message =
          "Module \"" ^ moduleName ^ "\" is missing from pathsForModule.";
      }
  | Some paths -> (
    let uri = Uri.fromPath sourcePath in
    let cmtPath = SharedTypes.getCmtPath ~uri paths in
    match CmtDirect.of_path ~moduleName ~path:cmtPath with
    | None ->
      Error
        {file = cmtPath; message = "Unable to read cmt/cmt[i] file for module."}
    | Some cmt -> Ok (cmtPath, cmt))

let canonicalize_path path =
  let path = try Unix.realpath path with _ -> path in
  if Sys.win32 then String.lowercase_ascii path else path

let path_is_within ~root path =
  path = root || Files.pathStartsWith path (root ^ Filename.dir_sep)

let source_is_selected ~includePaths ~excludePaths sourcePath =
  let sourcePath = canonicalize_path sourcePath in
  let included =
    includePaths = []
    || List.exists (fun root -> path_is_within ~root sourcePath) includePaths
  in
  let excluded =
    List.exists (fun root -> path_is_within ~root sourcePath) excludePaths
  in
  included && not excluded

let collect_gql_cmts ~sourceFolder ~includePaths ~excludePaths =
  let includePaths = List.map canonicalize_path includePaths in
  let excludePaths = List.map canonicalize_path excludePaths in
  match Packages.getPackage ~uri:(Uri.fromPath sourceFolder) with
  | None ->
    Error
      [
        {
          file = sourceFolder;
          message =
            "Source folder \"" ^ sourceFolder ^ "\" is not a ReScript project.";
        };
      ]
  | Some package ->
    let errs = ref [] in
    let loaded = ref [] in
    package.projectFiles
    |> FileSet.iter (fun modName ->
        match Hashtbl.find_opt package.pathsForModule modName with
        | None -> ()
        | Some paths ->
          (* Only consider implementation .res files for the initial gql attribute check. *)
          let sourcePaths = SharedTypes.getSrc paths in
          List.iter
            (fun sourcePath ->
              let hasAttr =
                source_is_selected ~includePaths ~excludePaths sourcePath
                &&
                  try GenerateSchemaUtils.fileHasGqlAttribute sourcePath
                  with _ -> false
              in
              if hasAttr then
                let moduleName =
                  BuildSystem.namespacedName package.namespace
                    (FindFiles.getName sourcePath)
                in
                match load_cmt ~package ~moduleName ~sourcePath with
                | Error err -> errs := err :: !errs
                | Ok (cmtPath, cmt) ->
                  loaded := {moduleName; sourcePath; cmtPath; cmt} :: !loaded)
            sourcePaths);
    if List.length !errs > 0 then Error (List.rev !errs)
    else Ok (package, List.rev !loaded)

let print_collect_errors errs =
  errs
  |> List.iter (fun {file; message} -> prerr_endline (file ^ ": " ^ message))

let with_hooks ~package ~preloaded f =
  let cache : (string, SharedTypes.File.t) Hashtbl.t = Hashtbl.create 100 in
  (* seed cache with already loaded summaries *)
  preloaded
  |> List.iter (fun (moduleName, file) -> Hashtbl.replace cache moduleName file);
  let load_and_cache moduleName =
    match Hashtbl.find_opt package.pathsForModule moduleName with
    | None -> None
    | Some paths -> (
      let uri = SharedTypes.getUri paths in
      let cmtPath = SharedTypes.getCmtPath ~uri paths in
      match CmtDirect.of_path ~moduleName ~path:cmtPath with
      | None -> None
      | Some cmt ->
        let file =
          CmtSummarize.file_from_cmt_infos ~moduleName ~uri
            (CmtDirect.infos cmt)
        in
        Hashtbl.replace cache moduleName file;
        Some file)
  in
  let loader ~moduleName =
    match Hashtbl.find_opt cache moduleName with
    | Some file -> Some file
    | None -> load_and_cache moduleName
  in
  let digHook = DirectReferences.digConstructor ~loader in
  References.setDigConstructorHook digHook;
  let res =
    try f ~loader
    with exn ->
      References.clearDigConstructorHook ();
      raise exn
  in
  References.clearDigConstructorHook ();
  res

let generateSchemaDirect ~printToStdOut ~writeStateFile ~sourceFolder ~debug
    ~outputFolder ~writeSdlFile ~schemaName ~moduleName ~contextType
    ~includePaths ~excludePaths ~authorizationConfig =
  let moduleOutputPaths =
    [
      outputFolder ^ "/" ^ moduleName ^ ".res";
      outputFolder ^ "/" ^ moduleName ^ ".resi";
    ]
  in
  GenerateSchemaAuthorization.validateBaselineOutputPath ~outputFolder
    ~writeSdlFile ~additionalOutputPaths:moduleOutputPaths authorizationConfig;
  let cacheEnabled =
    match authorizationConfig.mode with
    | AuthorizationOptional -> true
    | AuthorizationRequired | AuthorizationBaseline -> false
  in
  if
    cacheEnabled
    && GenerateSchemaCache.canSkip ~sourceFolder ~outputFolder ~writeStateFile
         ~writeSdlFile ~debug ~schemaName ~moduleName ~contextType ~includePaths
         ~excludePaths
  then (
    if printToStdOut then
      Printf.printf "{\"status\": \"Success\", \"ok\": true}")
  else
    let collection =
      try collect_gql_cmts ~sourceFolder ~includePaths ~excludePaths
      with exn ->
        GenerateSchemaAuthorization.prepareManifest ~outputFolder ~writeSdlFile
          ~additionalOutputPaths:moduleOutputPaths authorizationConfig;
        raise exn
    in
    match collection with
    | Error errs ->
      GenerateSchemaAuthorization.prepareManifest ~outputFolder ~writeSdlFile
        ~additionalOutputPaths:moduleOutputPaths authorizationConfig;
      print_collect_errors errs;
      exit 1
    | Ok (package, loaded) ->
      let additionalOutputPaths =
        moduleOutputPaths
        @
        if writeStateFile then
          [GenerateSchemaUtils.getStateFilePath ?schemaName package]
        else []
      in
      GenerateSchemaAuthorization.validateBaselineOutputPath ~outputFolder
        ~writeSdlFile ~additionalOutputPaths authorizationConfig;
      GenerateSchemaAuthorization.prepareManifest ~outputFolder ~writeSdlFile
        ~additionalOutputPaths authorizationConfig;
      let preloaded =
        loaded
        |> List.map (fun l ->
            let file =
              CmtSummarize.file_from_cmt_infos ~moduleName:l.moduleName
                ~uri:(Uri.fromPath l.sourcePath)
                (CmtDirect.infos l.cmt)
            in
            (l.moduleName, file))
      in
      ignore
        (with_hooks ~package ~preloaded (fun ~loader ->
             let projectConfigPath =
               let rescriptJson = package.rootPath ^ "/rescript.json" in
               if Files.exists rescriptJson then rescriptJson
               else package.rootPath ^ "/bsconfig.json"
             in
             let schemaState =
               {
                 contextTypePath = String.split_on_char '.' contextType;
                 rootFileUri = Uri.fromPath projectConfigPath;
                 types = Hashtbl.create 50;
                 enums = Hashtbl.create 10;
                 unions = Hashtbl.create 10;
                 inputObjects = Hashtbl.create 10;
                 inputUnions = Hashtbl.create 10;
                 interfaces = Hashtbl.create 10;
                 scalars = Hashtbl.create 10;
                 authorizationConfig;
                 authorizationDeclarations = Hashtbl.create 50;
                 authorizationPlans = Hashtbl.create 50;
                 resolverOutcomes = Hashtbl.create 20;
                 authorizationExemptions = Hashtbl.create 20;
                 authorizationGaps = [];
                 query = None;
                 subscription = None;
                 mutation = None;
                 diagnostics = [];
                 processedFiles = Hashtbl.create 100;
               }
             in

             preloaded
             |> List.iter (fun (_moduleName, file) ->
                 let full = {file; package} in
                 let env = SharedTypes.QueryEnv.fromFile file in
                 GenerateSchema.traverseStructure file.structure
                   ~originModule:env.file.moduleName ~schemaState ~env ~full
                   ~debug);

             let processedSchema =
               GenerateSchemaUtils.processSchema schemaState
             in
             GenerateSchemaAuthorization.buildPlans ~loader ~package schemaState;
             let markNamedSchemaFile contents =
               match schemaName with
               | Some _ ->
                 GenerateSchemaTypePrinters.markNamedSchemaFile contents
               | None -> contents
             in
             let schemaOutputPath = outputFolder ^ "/" ^ moduleName ^ ".res" in
             let resiOutputPath = schemaOutputPath ^ "i" in
             let resiContent =
               Printf.sprintf "let schema: ResGraph.schema<%s>\n" contextType
               |> markNamedSchemaFile
             in
             let sdlOutputPath = outputFolder ^ "/schema.graphql" in
             let interfaceModulePrefix =
               Option.map (fun _ -> moduleName) schemaName
             in

             (match schemaName with
             | Some _ ->
               GenerateSchemaTypePrinters.cleanNamedSchemaFiles ~outputFolder
                 ~moduleName;
               if not writeSdlFile then
                 GenerateSchemaTypePrinters.cleanNamedSchemaSdl ~outputFolder
             | None -> ());

             if schemaState.diagnostics |> List.length > 0 then (
               if printToStdOut then
                 Printf.printf
                   "{\n\
                   \  \"status\": \"Error\",\n\
                   \  \"errors\": \n\
                   \    [\n\
                   \      %s\n\
                   \    ]\n\
                    }"
                   (schemaState.diagnostics |> List.rev
                   |> List.map (fun (_, diagnostic) ->
                       GenerateSchemaUtils.printDiagnostic diagnostic)
                   |> String.concat ",\n");

               (* Write an empty schema just to avoid type errors in the generated code. *)
               GenerateSchemaUtils.writeIfHasChanges schemaOutputPath
                 (Printf.sprintf
                    "let schema: ResGraph.schema<%s> = \
                     ResGraph__GraphQLJs.GraphQLSchemaType.make(Obj.magic())\n"
                    contextType
                 |> markNamedSchemaFile);
               GenerateSchemaUtils.writeIfHasChanges resiOutputPath resiContent)
             else
               let schemaCode =
                 GenerateSchemaTypePrinters.printSchemaJsFile schemaState
                   processedSchema ~interfaceModulePrefix
                 |> markNamedSchemaFile
               in

               GenerateSchemaTypePrinters.cleanInterfaceFiles schemaState
                 ~outputFolder ~interfaceModulePrefix;
               GenerateSchemaTypePrinters.printInterfaceFiles schemaState
                 ~processedSchema ~outputFolder ~interfaceModulePrefix;

               if writeStateFile then
                 GenerateSchemaUtils.writeStateFile ?schemaName ~package
                   ~schemaState ~processedSchema ();

               (if writeSdlFile then
                  let sdl = GenerateSchemaSDL.printSchemaSDL schemaState in
                  let sdl =
                    match schemaName with
                    | Some _ ->
                      "# @generated by ResGraph named schema\n\n" ^ sdl
                    | None -> sdl
                  in
                  GenerateSchemaUtils.writeIfHasChanges sdlOutputPath sdl);

               GenerateSchemaUtils.writeIfHasChanges schemaOutputPath schemaCode;
               GenerateSchemaUtils.writeIfHasChanges resiOutputPath resiContent;

               GenerateSchemaAuthorization.writeBaseline schemaState;
               GenerateSchemaAuthorization.writeManifest ~package schemaState;

               if cacheEnabled then
                 GenerateSchemaCache.update ~package ~sourceFolder ~outputFolder
                   ~writeStateFile ~writeSdlFile ~debug ~schemaName ~moduleName
                   ~contextType ~includePaths ~excludePaths;

               if debug && printToStdOut then schemaCode |> print_endline
               else if printToStdOut then
                 Printf.printf "{\"status\": \"Success\", \"ok\": true}"))
