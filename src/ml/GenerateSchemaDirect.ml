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

let collect_gql_cmts ~sourceFolder =
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
    ~outputFolder ~writeSdlFile =
  match collect_gql_cmts ~sourceFolder with
  | Error errs ->
    print_collect_errors errs;
    exit 1
  | Ok (package, loaded) ->
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
      (with_hooks ~package ~preloaded (fun ~loader:_ ->
           let schemaState =
             {
               types = Hashtbl.create 50;
               enums = Hashtbl.create 10;
               unions = Hashtbl.create 10;
               inputObjects = Hashtbl.create 10;
               inputUnions = Hashtbl.create 10;
               interfaces = Hashtbl.create 10;
               scalars = Hashtbl.create 10;
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
           let schemaOutputPath = outputFolder ^ "/ResGraphSchema.res" in
           let sdlOutputPath = outputFolder ^ "/schema.graphql" in

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
               "let schema = \
                ResGraph__GraphQLJs.GraphQLSchemaType.make(Obj.magic())\n")
           else
             let schemaCode =
               GenerateSchemaTypePrinters.printSchemaJsFile schemaState
                 processedSchema
               |> GenerateSchemaUtils.formatCode ~debug
             in

             GenerateSchemaTypePrinters.cleanInterfaceFiles schemaState
               ~outputFolder;
             GenerateSchemaTypePrinters.printInterfaceFiles schemaState
               ~processedSchema ~outputFolder ~debug;

             (* TODO: Do this in parallell in some fancy way *)
             if writeStateFile then
               GenerateSchemaUtils.writeStateFile ~package ~schemaState
                 ~processedSchema;

             (if writeSdlFile then
                let sdl = GenerateSchemaSDL.printSchemaSDL schemaState in
                GenerateSchemaUtils.writeIfHasChanges sdlOutputPath sdl);

             (* Write generated schema *)
             GenerateSchemaUtils.writeIfHasChanges schemaOutputPath schemaCode;

             (* Write resi file *)
             let resiOutputPath = schemaOutputPath ^ "i" in
             let resiContent =
               "let schema: ResGraph.schema<ResGraphContext.context>\n"
             in
             GenerateSchemaUtils.writeIfHasChanges resiOutputPath resiContent;

             if debug && printToStdOut then schemaCode |> print_endline
             else if printToStdOut then
               Printf.printf "{\"status\": \"Success\", \"ok\": true}"))
