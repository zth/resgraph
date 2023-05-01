open GenerateSchemaTypes

let hashDiagnostic (diagnostic : diagnostic) =
  let diagnosticId =
    Printf.sprintf "%s%s%s"
      (diagnostic.fileUri |> Uri.toPath)
      (diagnostic.loc |> Loc.toString)
      diagnostic.message
  in
  let md5Hash = Digest.string diagnosticId in
  Digest.to_hex md5Hash

let addDiagnostic schemaState ~diagnostic =
  let hash = hashDiagnostic diagnostic in
  if schemaState.diagnostics |> List.exists (fun (h, _) -> h = hash) then ()
  else schemaState.diagnostics <- (hash, diagnostic) :: schemaState.diagnostics