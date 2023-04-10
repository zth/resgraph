type completables = Decorator of {label: string}

let cleanAttributeLabel ~text ~offsetStart ~offsetEnd =
  let rawLabel = String.sub text offsetStart (offsetEnd - offsetStart) in
  let ( ++ ) x y =
    match (x, y) with
    | Some i1, Some i2 -> Some (min i1 i2)
    | Some _, None -> x
    | None, _ -> y
  in
  let label =
    match
      String.index_opt rawLabel ' '
      ++ String.index_opt rawLabel '\t'
      ++ String.index_opt rawLabel '\r'
      ++ String.index_opt rawLabel '\n'
    with
    | None -> rawLabel
    | Some i -> String.sub rawLabel 0 i
  in
  if label <> "" && label.[0] = '@' then
    String.sub label 1 (String.length label - 1)
  else label

let completionWithParser ~debug ~path ~pos ~currentFile ~text =
  let posBeforeCursor = Pos.posBeforeCursor pos in
  let found = ref false in
  let result = ref None in
  let setResultOpt x =
    if !result = None then
      match x with
      | None -> ()
      | Some x -> result := Some x
  in
  let setResult x = setResultOpt (Some x) in
  let attribute (iterator : Ast_iterator.iterator)
      ((id, payload) : Parsetree.attribute) =
    (if String.length id.txt >= 4 && String.sub id.txt 0 4 = "res." then
     (* skip: internal parser attribute *) ()
    else if id.loc.loc_ghost then ()
    else if id.loc |> Loc.hasPos ~pos:posBeforeCursor then
      let posStart, posEnd = Loc.range id.loc in
      match
        (Pos.positionToOffset text posStart, Pos.positionToOffset text posEnd)
      with
      | Some offsetStart, Some offsetEnd ->
        (* Can't trust the parser's location
           E.g. @foo. let x... gives as label @foo.let *)
        let label = cleanAttributeLabel ~text ~offsetStart ~offsetEnd in
        found := true;
        if debug then
          Printf.printf "Attribute id:%s:%s label:%s\n" id.txt
            (Loc.toString id.loc) label;
        setResult (Decorator {label})
      | _ -> ());
    Ast_iterator.default_iterator.attribute iterator (id, payload)
  in
  let iterator = {Ast_iterator.default_iterator with attribute} in

  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
    in
    let {Res_driver.parsetree = str} = parser ~filename:currentFile in
    iterator.structure iterator str |> ignore;
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else if Filename.check_suffix path ".resi" then (
    let parser = Res_driver.parsingEngine.parseInterface ~forPrinter:false in
    let {Res_driver.parsetree = signature} = parser ~filename:currentFile in
    iterator.signature iterator signature |> ignore;
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else None

let getCompletions ~debug ~path ~pos ~currentFile =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> completionWithParser ~debug ~path ~pos ~currentFile ~text

let completion ~debug ~path ~pos ~currentFile =
  let completions =
    match getCompletions ~debug ~path ~pos ~currentFile with
    | None -> []
    | Some completion -> (
      match completion with
      | Decorator {label} ->
        GenerateSchemaUtils.validAttributes
        |> List.filter_map (fun (attrName, desc) ->
               if Utils.startsWith attrName label then
                 let completionItem : Protocol.completionItem =
                   {
                     label = attrName;
                     kind = 4;
                     tags = [];
                     detail = desc;
                     sortText = None;
                     filterText = None;
                     insertTextFormat = None;
                     insertText = None;
                     documentation = None;
                   }
                 in
                 Some completionItem
               else None))
  in
  Printf.printf "{\"status\": \"Completion\", \"items\": %s}"
    (completions |> List.map Protocol.stringifyCompletionItem |> Protocol.array)
