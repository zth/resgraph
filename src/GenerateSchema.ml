open GenerateSchemaTypes
open GenerateSchemaUtils

(* CONT:
   * Se till att de typerna du hittar finns i hashmappen.
   * Plocka upp resolvers via härleda return type från första argumentet i
     funktioner
*)

let rec findGraphQLType ~env ~state ~(full : SharedTypes.full)
    (typ : Types.type_expr) =
  (* TODO: Array/List *)
  match typ.desc with
  | Tlink te | Tsubst te | Tpoly (te, []) ->
    findGraphQLType te ~env ~state ~full
  | Tconstr (Path.Pident {name = "option"}, [unwrappedType], _) -> (
    let inner = findGraphQLType ~env ~state ~full unwrappedType in
    match inner with
    | None -> None
    | Some inner -> Some (Nullable inner))
  | Tconstr (Path.Pident {name = "promise"}, [unwrappedType], _) ->
    findGraphQLType unwrappedType ~env ~state ~full
  | Tconstr (Path.Pident {name = "string"}, [], _) -> Some (Scalar String)
  | Tconstr (Path.Pident {name = "bool"}, [], _) -> Some (Scalar Boolean)
  | Tconstr (Path.Pident {name = "int"}, [], _) -> Some (Scalar Int)
  | Tconstr (Path.Pident {name = "float"}, [], _) -> Some (Scalar Float)
  | Tconstr (path, _, _) -> (
    Printf.printf "%s\n" (pathIdentToList path |> String.concat ".");
    match pathIdentToList path with
    | ["ResGraph"; "id"] -> Some (Scalar ID)
    | ["Js"; ("String2" | "String"); "t"] ->
      (* TODO: Smarter way to resolve these... *) Some (Scalar String)
    | _ -> (
      (* If none of the above matches we'll see if we can dig to the underlying
         type, to make sure it's a valid GraphQL type. *)
      match References.digConstructor ~env ~package:full.package path with
      | Some (env, {item}) -> (
        match returnTypeFromItem item with
        | Some (GraphQLObjectType {name} as returnType) ->
          noticeObjectType name ~state;
          Some returnType
        | _ -> Some (Named {path; env}))
      | _ -> Some (Named {path; env})))
  | _ -> raise (Fail ("Invalid GraphQL type: " ^ Shared.typeToString typ))

let extractResolverFunctionInfo ~env ~(full : SharedTypes.full) ~(state : state)
    (typ : Types.type_expr) =
  match typ |> TypeUtils.extractType ~env ~package:full.package with
  | Some (Tfunction {typ; env}) -> (
    let args, returnType =
      TypeUtils.extractFunctionType ~env ~package:full.package typ
    in
    (* The first arg should point to the type this resolver is for. *)
    match List.nth_opt args 0 with
    | Some (Nolabel, typ) -> (
      Printf.printf "Checking for type to attaach resolver to\n";
      match
        ( findGraphQLType typ ~env ~full ~state,
          findGraphQLType returnType ~env ~full ~state )
      with
      | Some targetGraphQLType, Some returnType ->
        Some (targetGraphQLType, args, returnType)
      | _ -> None)
    | _ -> None)
  | _ -> None

let fieldsOfRecordFields ~env ~state ~(full : SharedTypes.full)
    (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         match field.attributes |> getFieldAttributeFromRawAttributes with
         | None -> None
         | Some attr -> Some (field.fname.txt, attr, field.typ))
  |> List.map (fun (name, _attr, typ) ->
         {
           name;
           resolverStyle = Property name;
           typ =
             getOrRaise
               (findGraphQLType typ ~full ~env ~state)
               (Printf.sprintf "Field %s had invalid GraphQL type." name);
           args = [];
         })

let printSchemaJsFile state =
  let code = ref "open ResGraph__GraphQLJs\n\n" in
  let addWithNewLine text = code := !code ^ text ^ "\n" in
  (* First print the type holders and getters *)
  state.types
  |> Hashtbl.iter (fun _name typ ->
         addWithNewLine
           (Printf.sprintf
              "let t_%s: ref<GraphQLObjectType.t> = Obj.magic({\"contents\": \
               Js.null})"
              typ.name);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => t_%s.contents" typ.name typ.name));

  addWithNewLine "";

  (* Now we can print all of the code that fills these in. *)
  state.types
  |> Hashtbl.iter (fun _name typ ->
         addWithNewLine
           (Printf.sprintf "t_%s.contents = GraphQLObjectType.make(%s)" typ.name
              (typ |> GenerateSchemaTypePrinters.printObjectType ~state)));

  (* Print the schema gluing it all together. *)
  addWithNewLine "";
  addWithNewLine "let schema = GraphQLSchemaType.make({\"query\": get_Query()})";
  !code

let rec traverseStructure ?(modulePath = []) ~state ~env ~full
    (structure : SharedTypes.Module.structure) =
  structure.items
  |> List.iter (fun (item : SharedTypes.Module.item) ->
         match (item.kind, item.attributes |> extractGqlAttribute) with
         | Module (Structure structure), _ ->
           traverseStructure
             ~modulePath:(structure.name :: modulePath)
             ~state ~env ~full structure
         | Type ({kind = Record fields}, _), Some ObjectType ->
           (* Records can be object types *)
           (* TODO: Add input objects, interfaces etc*)
           let name = capitalizeFirstChar item.name in
           let typ =
             {
               name;
               fields = fieldsOfRecordFields fields ~env ~full ~state:!state;
             }
           in
           Hashtbl.add !state.types name typ;
           if name = "Query" then state := {!state with query = Some typ}
         | Value typ, Some Field -> (
           (* Values with a field annotation could be a resolver. *)
           match typ |> TypeUtils.extractType ~env ~package:full.package with
           | Some (Tfunction {typ; env}) -> (
             Printf.printf "Found potential resolver fn: %s\n" item.name;
             match extractResolverFunctionInfo ~env ~full ~state:!state typ with
             | Some (GraphQLObjectType {name}, args, returnType) ->
               let field =
                 {
                   name = item.name;
                   resolverStyle =
                     Resolver
                       {
                         moduleName = env.file.moduleName;
                         fnName = item.name;
                         pathToFn = modulePath;
                       };
                   typ = returnType;
                   args =
                     args
                     |> List.filter_map (fun (label, typExpr) ->
                            match label with
                            | Asttypes.Nolabel -> None
                            | Labelled name | Optional name ->
                              Some
                                {
                                  name;
                                  typ =
                                    getOrRaise
                                      (findGraphQLType ~full ~env ~state:!state
                                         typExpr)
                                      (Printf.sprintf
                                         "%s is wrong GraphQL type." name);
                                });
                 }
               in
               addFieldToObjectType name ~field ~state:!state
             | _ -> ())
           | _ -> ())
         | _ -> ())

let generateSchema ~path ~debug ~outputPath =
  if debug then Printf.printf "generating schema from %s\n\n" path;
  match Cmt.loadFullCmtFromPath ~path with
  | None -> ()
  | Some full ->
    let file = full.file in
    let structure = file.structure in
    let open SharedTypes in
    let env = QueryEnv.fromFile file in
    let state = ref {types = Hashtbl.create 50; query = None} in
    traverseStructure structure ~state ~env ~full;
    let schemaCode = printSchemaJsFile !state |> formatCode in
    let oc = open_out outputPath in
    output_string oc schemaCode;
    close_out oc;
    if debug then schemaCode |> print_endline
