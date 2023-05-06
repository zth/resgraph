let instantiateType ~typeParams ~typeArgs (t : Types.type_expr) =
  if typeParams = [] || typeArgs = [] then t
  else
    let rec applySub tp ta t =
      match (tp, ta) with
      | t1 :: tRest1, t2 :: tRest2 ->
        if t1 = t then t2 else applySub tRest1 tRest2 t
      | [], _ | _, [] -> t
    in
    let rec loop (t : Types.type_expr) =
      match t.desc with
      | Tlink t -> loop t
      | Tvar _ -> applySub typeParams typeArgs t
      | Tunivar _ -> t
      | Tconstr (path, args, memo) ->
        {t with desc = Tconstr (path, args |> List.map loop, memo)}
      | Tsubst t -> loop t
      | Tvariant rd -> {t with desc = Tvariant (rowDesc rd)}
      | Tnil -> t
      | Tarrow (lbl, t1, t2, c) ->
        {t with desc = Tarrow (lbl, loop t1, loop t2, c)}
      | Ttuple tl -> {t with desc = Ttuple (tl |> List.map loop)}
      | Tobject (t, r) -> {t with desc = Tobject (loop t, r)}
      | Tfield (n, k, t1, t2) -> {t with desc = Tfield (n, k, loop t1, loop t2)}
      | Tpoly (t, []) -> loop t
      | Tpoly (t, tl) -> {t with desc = Tpoly (loop t, tl |> List.map loop)}
      | Tpackage (p, l, tl) ->
        {t with desc = Tpackage (p, l, tl |> List.map loop)}
    and rowDesc (rd : Types.row_desc) =
      let row_fields =
        rd.row_fields |> List.map (fun (l, rf) -> (l, rowField rf))
      in
      let row_more = loop rd.row_more in
      let row_name =
        match rd.row_name with
        | None -> None
        | Some (p, tl) -> Some (p, tl |> List.map loop)
      in
      {rd with row_fields; row_more; row_name}
    and rowField (rf : Types.row_field) =
      match rf with
      | Rpresent None -> rf
      | Rpresent (Some t) -> Rpresent (Some (loop t))
      | Reither (b1, tl, b2, r) -> Reither (b1, tl |> List.map loop, b2, r)
      | Rabsent -> Rabsent
    in
    loop t

let checkIfNoUninstantiatedVars (t : Types.type_expr) =
  let didInst = ref true in
  let hadArgs = ref false in
  let rec loop (t : Types.type_expr) =
    match t.desc with
    | Tlink t -> loop t
    | Tvar _ -> didInst := false
    | Tunivar _ -> didInst := false
    | Tconstr (_, args, _) ->
      args |> List.iter loop;
      if List.length args > 0 then hadArgs := true
    | Tsubst t -> loop t
    | Tvariant rd -> rowDesc rd
    | Tnil -> ()
    | Tarrow (_, t1, t2, _) ->
      loop t1;
      loop t2
    | Ttuple tl -> tl |> List.iter loop
    | Tobject (t, _) -> loop t
    | Tfield (_, _, t1, t2) ->
      loop t1;
      loop t2
    | Tpoly (t, []) -> loop t
    | Tpoly (t, tl) ->
      loop t;
      tl |> List.iter loop
    | Tpackage (_, _, tl) -> tl |> List.iter loop
  and rowDesc (rd : Types.row_desc) =
    rd.row_fields |> List.iter (fun (_, rf) -> rowField rf);
    loop rd.row_more;
    match rd.row_name with
    | None -> ()
    | Some (_, tl) -> tl |> List.iter loop
  and rowField (rf : Types.row_field) =
    match rf with
    | Rpresent None -> ()
    | Rpresent (Some t) -> loop t
    | Reither (_, tl, _, _) -> tl |> List.iter loop
    | Rabsent -> ()
  in
  loop t;
  !hadArgs && !didInst
