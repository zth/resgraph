open SharedTypes

let newHover ~full:{file; package} locItem =
  let schemaState =
    Lazy.from_fun (fun () -> GenerateSchemaUtils.readStateFile ~package |> fst)
  in

  match locItem.locType with
  | TypeDefinition (name, decl, _stamp) -> (
    let schemaState = Lazy.force schemaState in
    match
      GenerateSchemaUtils.extractGqlAttribute ~schemaState
        ~env:(QueryEnv.fromFile file) decl.type_attributes
    with
    | Some InputObject -> (
      match Hashtbl.find_opt schemaState.inputObjects name with
      | None -> None
      | Some input ->
        Some
          (Markdown.graphqlCodeBlock (GenerateSchemaSDL.printInputObject input))
      )
    | Some ObjectType -> (
      match Hashtbl.find_opt schemaState.types name with
      | None -> None
      | Some typ ->
        Some (Markdown.graphqlCodeBlock (GenerateSchemaSDL.printObjectType typ))
      )
    | Some Enum -> (
      match Hashtbl.find_opt schemaState.enums name with
      | None -> None
      | Some enum ->
        Some (Markdown.graphqlCodeBlock (GenerateSchemaSDL.printEnum enum)))
    | Some Union -> (
      match Hashtbl.find_opt schemaState.unions name with
      | None -> None
      | Some union ->
        Some (Markdown.graphqlCodeBlock (GenerateSchemaSDL.printUnion union)))
    | Some Scalar -> (
      match Hashtbl.find_opt schemaState.scalars name with
      | None -> None
      | Some scalar ->
        Some (Markdown.graphqlCodeBlock (GenerateSchemaSDL.printScalar scalar)))
    | Some Interface -> (
      match Hashtbl.find_opt schemaState.interfaces name with
      | None -> None
      | Some intf ->
        Some (Markdown.graphqlCodeBlock (GenerateSchemaSDL.printInterface intf))
      )
    | Some (InterfaceResolver _) | Some Field | None -> None)
  | Typed (_, _t, locKind) -> (
    match References.definedForLoc2 ~file ~package locKind with
    | None -> None
    | Some declared -> None
    | _ -> None)
  | _ -> None

let hover ~path ~pos ~debug =
  let result =
    match Cmt.loadFullCmtFromPath ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.getLocItem ~full ~pos ~debug with
      | None ->
        if debug then Printf.printf "Nothing at that position.\n";
        Protocol.null
      | Some locItem -> (
        let hoverText = newHover ~full locItem in
        match hoverText with
        | None -> Protocol.null
        | Some s -> Protocol.stringifyHover s))
  in
  Printf.printf "{\"status\": \"Hover\", \"item\": %s}" result