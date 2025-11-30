open SharedTypes

let digConstructor ~loader ~env ~package:_ path =
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
