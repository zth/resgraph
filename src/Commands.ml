let diagnosticSyntax ~path =
  print_endline (Diagnostics.document_syntax ~path |> Protocol.array)

let test ~path =
  Uri.stripPath := true;
  match Files.readFile path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let processLine i line =
      let createCurrentFile () =
        let currentFile, cout = Filename.open_temp_file "def" "txt" in
        let removeLineComment l =
          let len = String.length l in
          let rec loop i =
            if i + 2 <= len && l.[i] = '/' && l.[i + 1] = '/' then Some (i + 2)
            else if i + 2 < len && l.[i] = ' ' then loop (i + 1)
            else None
          in
          match loop 0 with
          | None -> l
          | Some indexAfterComment ->
            String.make indexAfterComment ' '
            ^ String.sub l indexAfterComment (len - indexAfterComment)
        in
        lines
        |> List.iteri (fun j l ->
               let lineToOutput =
                 if j == i - 1 then removeLineComment l else l
               in
               Printf.fprintf cout "%s\n" lineToOutput);
        close_out cout;
        currentFile
      in
      if Str.string_match (Str.regexp "^ *//[ ]*\\^") line 0 then
        let matched = Str.matched_string line in
        let len = line |> String.length in
        let mlen = String.length matched in
        let rest = String.sub line mlen (len - mlen) in
        let line = i - 1 in
        let col = mlen - 1 in
        if mlen >= 3 then (
          (match String.sub rest 0 3 with
          | "db+" -> Log.verbose := true
          | "db-" -> Log.verbose := false
          | "gen" ->
            GenerateSchema.generateSchema ~path ~debug:true
              ~outputPath:"./src/ResGraphSchema.res"
          | "ast" ->
            print_endline
              ("Dump AST " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let currentFile = createCurrentFile () in
            DumpAst.dump ~pos:(line, col) ~currentFile;
            Sys.remove currentFile
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri processLine
