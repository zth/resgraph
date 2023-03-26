(**
  TODO:
  - Dummy generate JS schema
*)

(* TODO: Granular exceptions. Loc-linked errors. All errors at the same time,
   not just the first encountered. *)
exception Fail of string

type scalar = Int | Float | String | Boolean | ID

(* TODO: List *)
type returnType =
  | Nullable of returnType
  | Scalar of scalar
  | Named of {path: Path.t; env: SharedTypes.QueryEnv.t}

let rec returnTypeToString returnType =
  match returnType with
  | Nullable inner -> "Nullable(" ^ returnTypeToString inner ^ ")"
  | Named {path} -> "Named(" ^ SharedTypes.pathIdentToString path ^ ")"
  | Scalar scalar -> (
    match scalar with
    | Int -> "Int"
    | Float -> "Float"
    | String -> "String"
    | Boolean -> "Boolean"
    | ID -> "ID")

let pathIdentToList (p : Path.t) =
  let rec pathIdentToListInner ?(acc = []) (p : Path.t) =
    match p with
    | Pident {name} -> name :: acc
    | Pdot (nextPath, id, _) -> [id] @ pathIdentToListInner ~acc nextPath
    | Papply _ -> acc
  in
  let lst = pathIdentToListInner p in
  lst |> List.rev

let rec toReturnType ~env (typ : Types.type_expr) =
  (* TODO: Array/List *)
  match typ.desc with
  | Tlink te | Tsubst te | Tpoly (te, []) -> toReturnType te ~env
  | Tconstr (Path.Pident {name = "option"}, [unwrappedType], _) ->
    let inner = toReturnType ~env unwrappedType in
    Nullable inner
  | Tconstr (Path.Pident {name = "promise"}, [unwrappedType], _) ->
    toReturnType unwrappedType ~env
  | Tconstr (Path.Pident {name = "string"}, [], _) -> Scalar String
  | Tconstr (Path.Pident {name = "bool"}, [], _) -> Scalar Boolean
  | Tconstr (Path.Pident {name = "int"}, [], _) -> Scalar Int
  | Tconstr (Path.Pident {name = "float"}, [], _) -> Scalar Float
  | Tconstr (path, _, _) -> (
    match pathIdentToList path with
    | ["ResGraph"; "id"] -> Scalar ID
    | ["Js"; ("String2" | "String"); "t"] ->
      (* TODO: Smarter way to resolve these... *) Scalar String
    | _ ->
      Printf.printf "++> %s\n" (SharedTypes.pathIdentToString path);
      Named {path; env})
  | _ ->
    raise (Fail ("Invalid return type of resolver: " ^ Shared.typeToString typ))

type fieldResolverStyle = Resolver | Property of string

type gqlArg = {name: string; typ: returnType (* TODO: Default value. *)}

type gqlField = {
  name: string;
  resolverStyle: fieldResolverStyle;
  typ: returnType;
  args: gqlArg list;
}

type typ = {name: string; fields: gqlField list}

type state = {types: (string, typ) Hashtbl.t; query: typ option}

type gqlAttributes = Type | Field

let extractAttributes (attributes : Parsetree.attributes) =
  attributes
  |> List.filter_map (fun ((name, _payload) : Parsetree.attribute) ->
         if Utils.startsWith name.txt "gql." then
           match String.split_on_char '.' name.txt with
           | ["gql"; "type"] -> Some Type
           | ["gql"; "field"] -> Some Field
           | _ -> (* TODO: Warn about invalid gql annotation*) None
         else None)

let getFieldAttribute gqlAttributes =
  gqlAttributes
  |> List.find_map (fun attr ->
         match attr with
         | Field -> Some attr
         | _ -> None)

let getFieldAttributeFromRawAttributes attributes =
  attributes |> extractAttributes |> getFieldAttribute

let fieldsOfType ~env ~package (structure : SharedTypes.Module.structure) =
  let open SharedTypes.Module in
  structure.items
  |> List.filter_map (fun item ->
         match item.kind with
         | Type ({name = "t"; kind = Record fields}, _) ->
           (* The main `t` type. Look for any fields with annotations. *)
           (* TODO: Resolve aliases *)
           Some
             (fields
             |> List.filter_map (fun (field : SharedTypes.field) ->
                    match
                      field.attributes |> getFieldAttributeFromRawAttributes
                    with
                    | None -> None
                    | Some attr -> Some (field.fname.txt, attr, field.typ))
             |> List.map (fun (name, _attr, typ) ->
                    {
                      name;
                      resolverStyle = Property name;
                      typ = toReturnType typ ~env;
                      args = [];
                    }))
         | Value typ -> (
           match typ |> TypeUtils.extractType ~env ~package with
           | Some (Tfunction {typ; env}) ->
             let args, returnType =
               TypeUtils.extractFunctionType ~env ~package typ
             in
             Some
               [
                 {
                   name = item.name;
                   resolverStyle = Resolver;
                   typ = toReturnType returnType ~env;
                   args =
                     args
                     |> List.filter_map (fun (label, typExpr) ->
                            match label with
                            | Asttypes.Nolabel -> None
                            | Labelled name | Optional name ->
                              Some {name; typ = toReturnType ~env typExpr});
                 };
               ]
           | _ -> None)
         | _ -> None)
  |> List.flatten

let debugPrintSchema state =
  match state.query with
  | None -> Printf.printf "Found no query\n"
  | Some _query ->
    state.types
    |> Hashtbl.iter (fun name typ ->
           Printf.printf "\n---\nFound type %s\nFields: \n%s\n" name
             (typ.fields
             |> List.map (fun (field : gqlField) ->
                    "# " ^ field.name ^ "\n  Type: "
                    ^ returnTypeToString field.typ
                    ^
                    if List.length field.args > 0 then
                      "\n  Args: "
                      ^ (field.args
                        |> List.map (fun (arg : gqlArg) ->
                               "[" ^ arg.name ^ ": "
                               ^ returnTypeToString arg.typ ^ "]")
                        |> String.concat ", ")
                    else
                      "" ^ "\n  Resolver style: "
                      ^
                      match field.resolverStyle with
                      | Property name -> "Property(" ^ name ^ ")"
                      | Resolver -> "Resolver")
             |> String.concat "\n"))

let generateSchema ~path ~debug =
  if debug then Printf.printf "generating schema from %s\n" path;
  match Cmt.loadFullCmtFromPath ~path with
  | None -> ()
  | Some full ->
    let file = full.file in
    let package = full.package in
    let structure = file.structure in
    let open SharedTypes in
    let env = QueryEnv.fromFile file in
    let state = ref {types = Hashtbl.create 50; query = None} in
    structure.items
    |> List.iter (fun (item : Module.item) ->
           match item.attributes |> extractAttributes with
           | [Type] -> (
             match item.kind with
             | Module (Structure ({name} as structure)) ->
               let typ =
                 {name; fields = fieldsOfType structure ~env ~package}
               in
               Hashtbl.add !state.types name typ;
               if name = "Query" then state := {!state with query = Some typ}
             | _ -> ())
           | _ -> ());

    debugPrintSchema !state
