let tryReadCmt cmt =
  if not (Files.exists cmt) then (
    Log.log ("Cmt file does not exist " ^ cmt);
    None)
  else
    match Cmt_format.read_cmt cmt with
    | exception Cmi_format.Error err ->
      let error_message =
        Cmi_format.report_error Format.str_formatter err;
        Format.flush_str_formatter ()
      in
      Log.log ("Invalid cmt format " ^ cmt ^ ": " ^ error_message);
      None
    | exception err ->
      Log.log ("Invalid cmt format " ^ cmt ^ ": " ^ Printexc.to_string err);
      None
    | x -> Some x

let rec dig (te : Types.type_expr) =
  match te.desc with
  | Tlink inner -> dig inner
  | Tsubst inner -> dig inner
  | Tpoly (inner, _) -> dig inner
  | _ -> te

let typeToString ?lineWidth (t : Types.type_expr) =
  PrintType.printExpr ?lineWidth t
