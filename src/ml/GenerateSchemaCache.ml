module StringSet = Set.Make (String)

type fileSignature = {
  path: string;
  mtime: float;
  ctime: float;
  size: int;
  kind: Unix.file_kind;
  symlinkTarget: string option;
}

type output = {signature: fileSignature; digest: string}

type t = {
  version: int;
  sourceFolder: string;
  outputFolder: string;
  writeStateFile: bool;
  writeSdlFile: bool;
  debug: bool;
  executable: fileSignature;
  inputs: fileSignature list;
  outputs: output list;
}

let version = 2
let magic = "RESGRAPH_INCREMENTAL_CACHE_V2\n"
let fileName = ".resgraphIncrementalCache"

let enabled () = Sys.getenv_opt "RESGRAPH_INCREMENTAL_CACHE" <> Some "false"

let log message =
  if Sys.getenv_opt "RESGRAPH_INCREMENTAL_DEBUG" = Some "1" then
    Printf.eprintf "%s\n%!" message

let canonicalize path =
  try Unix.realpath path
  with _ ->
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let fromRoot rootPath path =
  if Filename.is_relative path then Filename.concat rootPath path else path

let cachePath rootPath =
  Filename.concat (Filename.concat rootPath "lib") fileName

let signatureCanonical path =
  try
    let stat = Unix.lstat path in
    Some
      {
        path;
        mtime = stat.st_mtime;
        ctime = stat.st_ctime;
        size = stat.st_size;
        kind = stat.st_kind;
        symlinkTarget =
          (if stat.st_kind = Unix.S_LNK then Some (Unix.readlink path) else None);
      }
  with _ -> None

let signature path = signatureCanonical (canonicalize path)

let digest path = try Some (Digest.to_hex (Digest.file path)) with _ -> None

let rec findRoot path =
  let path =
    try if Sys.is_directory path then path else Filename.dirname path
    with _ -> Filename.dirname path
  in
  if
    Files.exists (Filename.concat path "rescript.json")
    || Files.exists (Filename.concat path "bsconfig.json")
  then Some path
  else
    let parent = Filename.dirname path in
    if parent = path then None else findRoot parent

let read path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let actualMagic = really_input_string channel (String.length magic) in
        if actualMagic <> magic then None
        else Some (Marshal.from_channel channel : t))
  with _ -> None

let write path cache =
  let tempPath = path ^ "." ^ string_of_int (Unix.getpid ()) ^ ".tmp" in
  try
    let channel = open_out_bin tempPath in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
        output_string channel magic;
        Marshal.to_channel channel cache []);
    Sys.rename tempPath path
  with _ -> ( try Sys.remove tempPath with _ -> ())

let rec addSymlinkAncestors path paths =
  let paths =
    match Unix.lstat path with
    | {Unix.st_kind = Unix.S_LNK} -> StringSet.add path paths
    | _ -> paths
    | exception _ -> paths
  in
  let parent = Filename.dirname path in
  if parent = path then paths else addSymlinkAncestors parent paths

let addPath path paths =
  let path = absolute path in
  let canonicalPath = canonicalize path in
  paths |> StringSet.add path
  |> StringSet.add canonicalPath
  |> addSymlinkAncestors path

let filesForModulePaths = function
  | SharedTypes.Impl {cmt; res} -> [cmt; res]
  | Namespace {cmt} -> [cmt]
  | IntfAndImpl {cmti; resi; cmt; res} -> [cmti; resi; cmt; res]

let configuredSourceRoots rootPath =
  let configPath =
    let rescriptJson = Filename.concat rootPath "rescript.json" in
    if Files.exists rescriptJson then rescriptJson
    else Filename.concat rootPath "bsconfig.json"
  in
  match Option.bind (Files.readFile configPath) Json.parse with
  | None -> []
  | Some config ->
    (let rec collect current item =
       match item with
       | Json.Array items -> items |> List.map (collect current) |> List.concat
       | Json.String path -> [(Filename.concat current path, false)]
       | Json.Object _ -> (
         match Option.bind (Json.get "dir" item) Json.string with
         | None -> []
         | Some dir -> (
           let path = Filename.concat current dir in
           match Json.get "subdirs" item with
           | Some Json.True -> [(path, true)]
           | Some (Json.Array _ as subdirs) | Some (Json.Object _ as subdirs) ->
             (path, false) :: collect path subdirs
           | _ -> [(path, false)]))
       | _ -> []
     in
     match Json.get "sources" config with
     | None -> []
     | Some sources -> collect "" sources)
    |> List.map (fun (path, recursive) -> (fromRoot rootPath path, recursive))
    |> List.map (fun (path, recursive) ->
        if recursive then Files.collectDirs path else [path])
    |> List.concat

let configPaths rootPath =
  [
    "rescript.json";
    "bsconfig.json";
    "package.json";
    "package-lock.json";
    "bun.lock";
    "pnpm-lock.yaml";
    "yarn.lock";
  ]
  |> List.map (Filename.concat rootPath)
  |> List.filter Files.exists

let inputPaths (package : SharedTypes.package) =
  let rootPath = canonicalize package.rootPath in
  let moduleFiles =
    Hashtbl.fold
      (fun _ modulePaths files -> filesForModulePaths modulePaths @ files)
      package.pathsForModule []
    |> List.map (fromRoot rootPath)
  in
  let dependencyRoots =
    (package.dependenciesFiles |> SharedTypes.FileSet.elements) @ moduleFiles
    |> List.filter_map findRoot
    |> List.filter (fun dependencyRoot -> dependencyRoot <> rootPath)
    |> List.sort_uniq String.compare
  in

  let dependencyConfigPaths =
    dependencyRoots
    |> List.map (fun dependencyRoot ->
        dependencyRoot :: configPaths dependencyRoot)
    |> List.concat
  in
  let files = configPaths rootPath @ dependencyConfigPaths @ moduleFiles in
  let paths =
    rootPath :: files
    |> List.fold_left (fun paths path -> addPath path paths) StringSet.empty
  in
  let paths =
    moduleFiles
    |> List.fold_left
         (fun paths path -> addPath (Filename.dirname path) paths)
         paths
  in
  configuredSourceRoots rootPath
  |> List.fold_left (fun paths path -> addPath path paths) paths
  |> StringSet.elements

let isInterfaceFile name =
  String.starts_with name ~prefix:"interface_"
  && Filename.check_suffix name ".res"

let generatedOutputPaths ~rootPath ~outputFolder ~writeStateFile ~writeSdlFile =
  let files = try Sys.readdir outputFolder |> Array.to_list with _ -> [] in
  let generated =
    files
    |> List.filter (fun name ->
        name = "ResGraphSchema.res"
        || name = "ResGraphSchema.resi"
        || (writeSdlFile && name = "schema.graphql")
        || isInterfaceFile name)
    |> List.map (Filename.concat outputFolder)
  in
  (if writeStateFile then
     Filename.concat (Filename.concat rootPath "lib") ".resgraphState.marshal"
     :: generated
   else generated)
  |> List.map canonicalize |> List.sort String.compare

let collectSignatures paths =
  let rec loop collected = function
    | [] -> Some (List.rev collected)
    | path :: rest -> (
      match signatureCanonical path with
      | None -> None
      | Some signature -> loop (signature :: collected) rest)
  in
  loop [] paths

let collectOutputs paths =
  let rec loop collected = function
    | [] -> Some (List.rev collected)
    | path :: rest -> (
      match (signatureCanonical path, digest path) with
      | Some signature, Some digest ->
        loop ({signature; digest} :: collected) rest
      | _ -> None)
  in
  loop [] paths

let validateInputs inputs =
  List.for_all
    (fun expected -> signatureCanonical expected.path = Some expected)
    inputs

let validateOutputs outputs =
  let rec loop changed validated = function
    | [] -> Some (changed, List.rev validated)
    | output :: rest -> (
      match signatureCanonical output.signature.path with
      | None -> None
      | Some current when current = output.signature ->
        loop changed (output :: validated) rest
      | Some current -> (
        match digest current.path with
        | Some currentDigest when currentDigest = output.digest ->
          loop true
            ({signature = current; digest = currentDigest} :: validated)
            rest
        | _ -> None))
  in
  loop false [] outputs

let invalid reason =
  log ("Incremental cache miss: " ^ reason);
  false

let canSkipEnabled ~sourceFolder ~outputFolder ~writeStateFile ~writeSdlFile
    ~debug:debugMode =
  match findRoot (canonicalize sourceFolder) with
  | None -> invalid "project root was not found"
  | Some rootPath -> (
    let path = cachePath rootPath in
    match read path with
    | None -> invalid "cache was not found or could not be read"
    | Some cache -> (
      let outputPaths =
        generatedOutputPaths ~rootPath ~outputFolder ~writeStateFile
          ~writeSdlFile
      in
      let cachedOutputPaths =
        cache.outputs
        |> List.map (fun output -> output.signature.path)
        |> List.sort String.compare
      in
      if cache.version <> version then invalid "cache version changed"
      else if cache.sourceFolder <> canonicalize sourceFolder then
        invalid "source folder changed"
      else if cache.outputFolder <> canonicalize outputFolder then
        invalid "output folder changed"
      else if cache.writeStateFile <> writeStateFile then
        invalid "state-file setting changed"
      else if cache.writeSdlFile <> writeSdlFile then
        invalid "SDL setting changed"
      else if cache.debug <> debugMode then invalid "debug setting changed"
      else if signature Sys.executable_name <> Some cache.executable then
        invalid "ResGraph executable changed"
      else if cachedOutputPaths <> outputPaths then
        invalid "generated output set changed"
      else if not (validateInputs cache.inputs) then
        invalid "project input changed"
      else
        match validateOutputs cache.outputs with
        | None -> invalid "generated output changed"
        | Some (outputsChanged, outputs) ->
          if outputsChanged then write path {cache with outputs};
          log "Incremental cache hit";
          true))

let canSkip ~sourceFolder ~outputFolder ~writeStateFile ~writeSdlFile ~debug =
  enabled ()
  && canSkipEnabled ~sourceFolder ~outputFolder ~writeStateFile ~writeSdlFile
       ~debug

let update ~(package : SharedTypes.package) ~sourceFolder ~outputFolder
    ~writeStateFile ~writeSdlFile ~debug:debugMode =
  if enabled () then
    let rootPath = canonicalize package.rootPath in
    let inputs = collectSignatures (inputPaths package) in
    let outputPaths =
      generatedOutputPaths ~rootPath ~outputFolder ~writeStateFile ~writeSdlFile
    in
    let outputs = collectOutputs outputPaths in
    match (signature Sys.executable_name, inputs, outputs) with
    | Some executable, Some inputs, Some outputs ->
      write (cachePath rootPath)
        {
          version;
          sourceFolder = canonicalize sourceFolder;
          outputFolder = canonicalize outputFolder;
          writeStateFile;
          writeSdlFile;
          debug = debugMode;
          executable;
          inputs;
          outputs;
        }
    | _ -> log "Incremental cache not written because an input was missing"
