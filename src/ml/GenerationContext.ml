type t = {
  cmts: (string, CmtDirect.t option) Hashtbl.t;
  summaries: (string, SharedTypes.File.t option) Hashtbl.t;
}

let create () = {cmts = Hashtbl.create 128; summaries = Hashtbl.create 128}

let canonicalize path =
  let path =
    try Unix.realpath path
    with _ ->
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
      else path
  in
  if Sys.win32 then String.lowercase_ascii path else path

let summaryKey (package : SharedTypes.package) moduleName =
  canonicalize package.rootPath ^ "\000" ^ moduleName

let loadCmt context ~moduleName ~path =
  let key = canonicalize path in
  match Hashtbl.find_opt context.cmts key with
  | Some result -> result
  | None ->
    let result = CmtDirect.of_path ~moduleName ~path in
    Hashtbl.replace context.cmts key result;
    result

let seedSummary context ~(package : SharedTypes.package) ~moduleName file =
  Hashtbl.replace context.summaries (summaryKey package moduleName) (Some file)

let loadSummary context ~(package : SharedTypes.package) ~moduleName =
  let key = summaryKey package moduleName in
  match Hashtbl.find_opt context.summaries key with
  | Some result -> result
  | None ->
    let result =
      match Hashtbl.find_opt package.pathsForModule moduleName with
      | None -> None
      | Some paths ->
        let uri = SharedTypes.getUri paths in
        let cmtPath = SharedTypes.getCmtPath ~uri paths in
        Option.map
          (fun cmt ->
            CmtSummarize.file_from_cmt_infos ~moduleName ~uri
              (CmtDirect.infos cmt))
          (loadCmt context ~moduleName ~path:cmtPath)
    in
    Hashtbl.replace context.summaries key result;
    result
