open SharedTypes

type loader = moduleName:string -> File.t option

type resolution =
  | Exported of QueryEnv.t * filePath
  | Global of filePath * filePath list
  | GlobalMod of filePath
  | NotFound
  | Stamp of int

let rec joinPaths modulePath path =
  match modulePath with
  | Path.Pident ident -> (ident.stamp, ident.name, path)
  | Papply (fnPath, _argPath) -> joinPaths fnPath path
  | Pdot (inner, name, _) -> joinPaths inner (name :: path)

let rec makePath ~(env : QueryEnv.t) modulePath =
  match modulePath with
  | Path.Pident ident when ident.stamp == 0 -> GlobalMod ident.name
  | Pident ident -> Stamp ident.stamp
  | Papply (fnPath, _argPath) -> makePath ~env fnPath
  | Pdot (inner, name, _) -> (
    match joinPaths inner [name] with
    | 0, moduleName, path -> Global (moduleName, path)
    | stamp, _moduleName, path -> (
      let res =
        match Stamps.findModule env.file.stamps stamp with
        | None -> None
        | Some {item = kind} -> findInModule ~env kind path
      in
      match res with
      | None -> NotFound
      | Some (`Local (env, name)) -> Exported (env, name)
      | Some (`Global (moduleName, fullPath)) -> Global (moduleName, fullPath)))

and resolvePathInner ~(env : QueryEnv.t) ~path =
  match path with
  | [] -> None
  | [name] -> Some (`Local (env, name))
  | subName :: subPath -> (
    match Exported.find env.exported Exported.Module subName with
    | None -> None
    | Some stamp -> (
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some {item} -> findInModule ~env item subPath))

and findInModule ~(env : QueryEnv.t) module_ path =
  match module_ with
  | Structure structure ->
    resolvePathInner ~env:(QueryEnv.enterStructure env structure) ~path
  | Constraint (_, module1) -> findInModule ~env module1 path
  | Ident modulePath -> (
    let stamp, moduleName, fullPath = joinPaths modulePath path in
    if stamp = 0 then Some (`Global (moduleName, fullPath))
    else
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some {item} -> findInModule ~env item fullPath)

let rec resolvePath ~env ~path ~loader =
  match resolvePathInner ~env ~path with
  | None -> None
  | Some result -> (
    match result with
    | `Local (env, name) -> Some (env, name)
    | `Global (moduleName, fullPath) -> (
      match loader ~moduleName with
      | None -> None
      | Some file ->
        resolvePath ~env:(QueryEnv.fromFile file) ~path:fullPath ~loader))

let fromCompilerPath ~(env : QueryEnv.t) path : resolution =
  match makePath ~env path with
  | Stamp stamp -> Stamp stamp
  | GlobalMod name -> GlobalMod name
  | NotFound -> NotFound
  | Exported (env, name) -> Exported (env, name)
  | Global (moduleName, fullPath) -> Global (moduleName, fullPath)

let resolveModuleFromCompilerPath ~env ~loader path =
  match fromCompilerPath ~env path with
  | Global (moduleName, path) -> (
    match loader ~moduleName with
    | None -> None
    | Some file -> (
      let env = QueryEnv.fromFile file in
      match resolvePath ~env ~loader ~path with
      | None -> None
      | Some (env, name) -> (
        match Exported.find env.exported Exported.Module name with
        | None -> None
        | Some stamp -> (
          match Stamps.findModule env.file.stamps stamp with
          | None -> None
          | Some declared -> Some (env, Some declared)))))
  | Stamp stamp -> (
    match Stamps.findModule env.file.stamps stamp with
    | None -> None
    | Some declared -> Some (env, Some declared))
  | GlobalMod moduleName -> (
    match loader ~moduleName with
    | None -> None
    | Some file ->
      let env = QueryEnv.fromFile file in
      Some (env, None))
  | NotFound -> None
  | Exported (env, name) -> (
    match Exported.find env.exported Exported.Module name with
    | None -> None
    | Some stamp -> (
      match Stamps.findModule env.file.stamps stamp with
      | None -> None
      | Some declared -> Some (env, Some declared)))

let resolveFromCompilerPath ~env ~loader path =
  match fromCompilerPath ~env path with
  | Global (moduleName, path) -> (
    let res =
      match loader ~moduleName with
      | None -> None
      | Some file ->
        let env = QueryEnv.fromFile file in
        resolvePath ~env ~loader ~path
    in
    match res with
    | None -> NotFound
    | Some (env, name) -> Exported (env, name))
  | Stamp stamp -> Stamp stamp
  | GlobalMod _ -> NotFound
  | NotFound -> NotFound
  | Exported (env, name) -> Exported (env, name)
