(**
 * `startsWith(string, prefix)`
 * true if the string starts with the prefix
 *)
let startsWith s prefix =
  if prefix = "" then true
  else
    let p = String.length prefix in
    p <= String.length s && String.sub s 0 p = prefix

let endsWith s suffix =
  if suffix = "" then true
  else
    let p = String.length suffix in
    let l = String.length s in
    p <= String.length s && String.sub s (l - p) p = suffix

let isFirstCharUppercase s =
  String.length s > 0 && Char.equal s.[0] (Char.uppercase_ascii s.[0])

let cmtPosToPosition {Lexing.pos_lnum; pos_cnum; pos_bol} =
  Protocol.{line = pos_lnum - 1; character = pos_cnum - pos_bol}

let cmtLocToRange {Location.loc_start; loc_end} =
  Protocol.{start = cmtPosToPosition loc_start; end_ = cmtPosToPosition loc_end}

let endOfLocation loc length =
  let open Location in
  {
    loc with
    loc_start = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - length};
  }

let chopLocationEnd loc length =
  let open Location in
  {
    loc with
    loc_end = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - length};
  }

(** An optional List.find *)
let rec find fn items =
  match items with
  | [] -> None
  | one :: rest -> (
    match fn one with
    | None -> find fn rest
    | Some x -> Some x)

let filterMap f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l -> (
      match f x with
      | None -> aux accu l
      | Some v -> aux (v :: accu) l)
  in
  aux []

let dumpPath path = Str.global_replace (Str.regexp_string "\\") "/" path
let isUncurriedInternal path = startsWith (Path.name path) "Js.Fn.arity"

let flattenLongIdent ?(jsx = false) ?(cutAtOffset = None) lid =
  let extendPath s path =
    match path with
    | "" :: _ -> path
    | _ -> s :: path
  in
  let rec loop lid =
    match lid with
    | Longident.Lident txt -> ([txt], String.length txt)
    | Ldot (lid, txt) ->
      let path, offset = loop lid in
      if Some offset = cutAtOffset then (extendPath "" path, offset + 1)
      else if jsx && txt = "createElement" then (path, offset)
      else if txt = "_" then (extendPath "" path, offset + 1)
      else (extendPath txt path, offset + 1 + String.length txt)
    | Lapply _ -> ([], 0)
  in
  let path, _ = loop lid in
  List.rev path

(* unused helpers removed *)

let rec skipWhite text i =
  if i < 0 then 0
  else
    match text.[i] with
    | ' ' | '\n' | '\r' | '\t' -> skipWhite text (i - 1)
    | _ -> i

let hasBraces attributes =
  attributes |> List.exists (fun (loc, _) -> loc.Location.txt = "res.braces")

let rec unwrapIfOption (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> unwrapIfOption t1
  | Tconstr (Path.Pident {name = "option"}, [unwrappedType], _) -> unwrappedType
  | _ -> t

let isJsxComponent (vb : Parsetree.value_binding) =
  vb.pvb_attributes
  |> List.exists (function
       | {Location.txt = "react.component" | "jsx.component"}, _payload -> true
       | _ -> false)

let checkName name ~prefix ~exact =
  if exact then name = prefix else startsWith name prefix

let rec getUnqualifiedName txt =
  match txt with
  | Longident.Lident fieldName -> fieldName
  | Ldot (t, _) -> getUnqualifiedName t
  | _ -> ""

let mkPosition (pos : Pos.t) =
  let line, character = pos in
  {Protocol.line; character}

let rangeOfLoc (loc : Location.t) =
  let start = loc |> Loc.start |> mkPosition in
  let end_ = loc |> Loc.end_ |> mkPosition in
  {Protocol.start; end_}

let rec expandPath (path : Path.t) =
  match path with
  | Pident id -> [Ident.name id]
  | Pdot (p, s, _) -> s :: expandPath p
  | Papply _ -> []

module Option = struct
  let flatMap f o =
    match o with
    | None -> None
    | Some v -> f v
end

let rec lastElements list =
  match list with
  | ([_; _] | [_] | []) as res -> res
  | _ :: tl -> lastElements tl

let lowercaseFirstChar s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.lowercase_ascii c else c) s

let cutAfterDash s =
  match String.index s '-' with
  | n -> ( try String.sub s 0 n with Invalid_argument _ -> s)
  | exception Not_found -> s

let fileNameHasUnallowedChars s =
  let regexp = Str.regexp "[^A-Za-z0-9_]" in
  try
    ignore (Str.search_forward regexp s 0);
    true
  with Not_found -> false

(* Flattens any namespace in the provided path.
   Example:
    Globals-RescriptBun.URL.t (which is an illegal path because of the namespace) becomes:
    RescriptBun.Globals.URL.t
*)
let rec flattenAnyNamespaceInPath path =
  match path with
  | [] -> []
  | head :: tail ->
    if String.contains head '-' then
      let parts = String.split_on_char '-' head in
      (* Namespaces are in reverse order, so "URL-RescriptBun" where RescriptBun is the namespace. *)
      (parts |> List.rev) @ flattenAnyNamespaceInPath tail
    else head :: flattenAnyNamespaceInPath tail

let printMaybeExoticIdent ?(allowUident = false) txt =
  let len = String.length txt in

  let rec loop i =
    if i == len then txt
    else if i == 0 then
      match String.unsafe_get txt i with
      | 'A' .. 'Z' when allowUident -> loop (i + 1)
      | 'a' .. 'z' | '_' -> loop (i + 1)
      | _ -> "\"" ^ txt ^ "\""
    else
      match String.unsafe_get txt i with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '\'' | '_' -> loop (i + 1)
      | _ -> "\"" ^ txt ^ "\""
  in
  if Res_token.is_keyword_txt txt then "\"" ^ txt ^ "\"" else loop 0

let findPackageJson root =
  let path = Uri.toPath root in

  let rec loop path =
    if path = "/" then None
    else if Files.exists (Filename.concat path "package.json") then
      Some (Filename.concat path "package.json")
    else
      let parent = Filename.dirname path in
      if parent = path then (* reached root *) None else loop parent
  in
  loop path
