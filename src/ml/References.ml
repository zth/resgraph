open SharedTypes

(* Optional override for resolving constructors; used by the direct CMT pipeline. *)
let digConstructorHook :
    (env:QueryEnv.t -> package:package -> Path.t ->
    (QueryEnv.t * Type.t Declared.t) option)
    option ref =
  ref None

(* Default loader cache keyed by package root so we don't re-read cmts repeatedly. *)
let defaultLoaderCache :
    (string, moduleName:string -> File.t option) Hashtbl.t =
  Hashtbl.create 3

let loaderForPackage (package : package) =
  match Hashtbl.find_opt defaultLoaderCache package.rootPath with
  | Some loader -> loader
  | None ->
    let cache = Hashtbl.create 50 in
    let loader ~moduleName =
      match Hashtbl.find_opt cache moduleName with
      | Some file -> Some file
      | None ->
        match Hashtbl.find_opt package.pathsForModule moduleName with
        | None -> None
        | Some paths ->
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
            Some file
    in
    Hashtbl.replace defaultLoaderCache package.rootPath loader;
    loader

let setDigConstructorHook hook = digConstructorHook := Some hook
let clearDigConstructorHook () = digConstructorHook := None

let digConstructor ~env ~package path =
  match !digConstructorHook with
  | Some hook -> hook ~env ~package path
  | None ->
    let loader = loaderForPackage package in
    match DirectResolve.resolveFromCompilerPath ~env ~loader path with
    | DirectResolve.NotFound -> None
    | Stamp stamp -> (
      match Stamps.findType env.file.stamps stamp with
      | None -> None
      | Some t -> Some (env, t))
    | Exported (env, name) -> (
      match Exported.find env.exported Exported.Type name with
      | None -> None
      | Some stamp -> (
        match Stamps.findType env.file.stamps stamp with
        | None -> None
        | Some t -> Some (env, t)))
    | _ -> None
