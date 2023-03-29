open GenerateSchemaTypes
open GenerateSchemaUtils

(* CONT:
   * Se till att de typerna du hittar finns i hashmappen.
   * Plocka upp resolvers via härleda return type från första argumentet i
     funktioner
*)

let variantCasesToEnumValues (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.map (fun (case : SharedTypes.Constructor.t) ->
         {
           value = case.cname.txt;
           description = case.attributes |> attributesToDocstring;
           deprecationReason = case.deprecated;
           loc = case.cname.loc;
         })

let rec findGraphQLType ~env ~state ~(full : SharedTypes.full)
    (typ : Types.type_expr) =
  match typ.desc with
  | Tlink te | Tsubst te | Tpoly (te, []) ->
    findGraphQLType te ~env ~state ~full
  | Tconstr (Path.Pident {name = "option"}, [unwrappedType], _) -> (
    let inner = findGraphQLType ~env ~state ~full unwrappedType in
    match inner with
    | None -> None
    | Some inner -> Some (Nullable inner))
  | Tconstr (Path.Pident {name = "array"}, [unwrappedType], _) -> (
    let inner = findGraphQLType ~env ~state ~full unwrappedType in
    match inner with
    | None -> None
    | Some inner -> Some (List inner))
  | Tconstr (Path.Pident {name = "promise"}, [unwrappedType], _) ->
    findGraphQLType unwrappedType ~env ~state ~full
  | Tconstr (Path.Pident {name = "string"}, [], _) -> Some (Scalar String)
  | Tconstr (Path.Pident {name = "bool"}, [], _) -> Some (Scalar Boolean)
  | Tconstr (Path.Pident {name = "int"}, [], _) -> Some (Scalar Int)
  | Tconstr (Path.Pident {name = "float"}, [], _) -> Some (Scalar Float)
  | Tconstr (path, typeArgs, _) -> (
    match pathIdentToList path with
    | ["ResGraph"; "id"] -> Some (Scalar ID)
    | ["ResGraphContext"; "context"] -> Some InjectContext
    | ["Js"; "Nullable"; "t"] -> (
      match typeArgs with
      | [typeArg] -> (
        let inner = findGraphQLType ~env ~state ~full typeArg in
        match inner with
        | None -> None
        | Some inner -> Some (RescriptNullable inner))
      | _ -> None)
    | _ -> (
      (* If none of the above matches we'll see if we can dig to the underlying
         type, to make sure it's a valid GraphQL type. *)
      match References.digConstructor ~env ~package:full.package path with
      | Some (env, {item = {decl = {type_params; type_manifest = Some te}}}) ->
        let typeParams = type_params in
        let te = TypeUtils.instantiateType ~typeParams ~typeArgs te in
        findGraphQLType te ~env ~state ~full
      | Some (env, {item}) -> (
        match (graphqlTypeFromItem item, item.kind) with
        | Some (GraphQLObjectType {id} as graphqlType), _ ->
          noticeObjectType id ~state ~env ~loc:item.decl.type_loc |> ignore;
          Some graphqlType
        | Some (GraphQLInputObject _ as graphqlType), _ ->
          (* TODO: Add here? *) Some graphqlType
        | Some (GraphQLEnum {id} as graphqlType), Variant cases ->
          addEnum id ~state ~makeEnum:(fun () ->
              {
                id;
                displayName = capitalizeFirstChar id;
                values = variantCasesToEnumValues cases;
                description = item.attributes |> attributesToDocstring;
                loc = item.decl.type_loc;
              });
          Some graphqlType
        | Some (GraphQLUnion {id} as graphqlType), Variant cases ->
          addUnion id ~state ~makeUnion:(fun () ->
              {
                id;
                displayName = capitalizeFirstChar id;
                types = variantCasesToUnionValues cases ~env ~full ~state;
                description = item.attributes |> attributesToDocstring;
                typeLocation =
                  findTypeLocation ~loc:item.decl.type_loc ~env
                    ~expectedType:Union id;
              });
          Some graphqlType
        | _ -> Some (Named {path; env}))
      | _ -> Some (Named {path; env})))
  | _ -> raise (Fail ("Invalid GraphQL type: " ^ Shared.typeToString typ))

and variantCasesToUnionValues ~env ~state ~full
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | Args [(typ, _)] -> (
           match findGraphQLType ~env ~state ~full typ with
           | Some (GraphQLObjectType {id; displayName}) ->
             Some {objectTypeId = id; displayName; loc = case.cname.loc}
           | _ ->
             addDiagnostic state
               ~diagnostic:
                 {
                   loc = case.cname.loc;
                   message =
                     Printf.sprintf
                       "The payload of the variant member `%s` of the variant \
                        `%s` is not a GraphL object. The payload needs to be a \
                        type representing a GraphQL object, meaning it's \
                        annotated with @gql.type."
                       case.cname.txt (case.typeDecl |> fst);
                   fileUri = env.file.uri;
                 };
             None)
         | _ -> None)

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
      Printf.printf "Checking for type to attach resolver to\n";
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
         | Some attr -> Some (field, attr))
  |> List.map (fun ((field : SharedTypes.field), _attr) ->
         let name = field.fname.txt in
         {
           name;
           resolverStyle = Property name;
           typ =
             getOrRaise
               (findGraphQLType field.typ ~full ~env ~state)
               (Printf.sprintf "Field %s had invalid GraphQL type." name);
           args = [];
           description = field.attributes |> attributesToDocstring;
           deprecationReason = field.deprecated;
           loc = field.fname.loc;
         })

let inputObjectfieldsOfRecordFields ~env ~state ~(full : SharedTypes.full)
    (fields : SharedTypes.field list) =
  fields
  |> List.map (fun (field : SharedTypes.field) ->
         let name = field.fname.txt in
         {
           name;
           resolverStyle = Property name;
           typ =
             getOrRaise
               (findGraphQLType field.typ ~full ~env ~state)
               (Printf.sprintf "Field %s had invalid GraphQL type." name);
           args = [];
           description = field.attributes |> attributesToDocstring;
           deprecationReason = field.deprecated;
           loc = field.fname.loc;
         })

let printSchemaJsFile state =
  let code = ref "@@warning(\"-27\")\n\nopen ResGraph__GraphQLJs\n\n" in
  let addWithNewLine text = code := !code ^ text ^ "\n" in
  (* Add the type unwrapper. Source types passed to resolvers might be either
     objects or variant cases. This is because we rely on variants for unions
     and interfaces. Variant cases are boxed, so they need to be unwrapped
     before they're passed to the resolver the developer has defined.
     `typeUnwrapper` unwraps any variant case to its specified object.
  *)
  addWithNewLine
    "let typeUnwrapper: ('src) => 'return = %raw(`function typeUnwrapper(src) \
     { if (src == null) return null; if (typeof src === 'object' && \
     src.hasOwnProperty('_0')) return src['_0']; return src;}`)";

  (* Add conversion assets. *)
  addWithNewLine
    "type inputObjectFieldConverterFn; external \
     makeInputObjectFieldConverterFn: ('a => 'b) => \
     inputObjectFieldConverterFn = \"%identity\";";

  addWithNewLine
    {|

let applyConversionToInputObject: ('a, array<(string, inputObjectFieldConverterFn)>) => 'a = %raw(`function applyConversionToInputObject(obj, instructions) {
  if (instructions.length === 0) return obj;
  let newObj = Object.assign({}, obj);
  instructions.forEach(instruction => {
    let value = newObj[instruction[0]];
     newObj[instruction[0]] = instruction[1](value);
  })
  return newObj;
}`)

|};

  (* Print all enums. These won't have any other dependencies, so they can be printed as is. *)
  state.enums
  |> Hashtbl.iter (fun _name (enum : gqlEnum) ->
         addWithNewLine
           (Printf.sprintf
              "let enum_%s = GraphQLEnumType.make({name: \"%s\", description: \
               %s, values: {%s}->makeEnumValues})"
              enum.displayName enum.displayName
              (undefinedOrValueAsString enum.description)
              (enum.values
              |> List.map (fun (v : gqlEnumValue) ->
                     Printf.sprintf
                       "\"%s\": {GraphQLEnumType.value: \"%s\", description: \
                        %s, deprecationReason: %s}"
                       v.value v.value
                       (undefinedOrValueAsString v.description)
                       (undefinedOrValueAsString v.deprecationReason))
              |> String.concat ", ")));

  (* Print the object type holders and getters *)
  state.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
         addWithNewLine
           (Printf.sprintf
              "let t_%s: ref<GraphQLObjectType.t> = Obj.magic({\"contents\": \
               Js.null})"
              typ.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => t_%s.contents" typ.displayName
              typ.displayName));

  (* Print the input object type holders and getters *)
  state.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
         addWithNewLine
           (Printf.sprintf
              "let input_%s: ref<GraphQLInputObjectType.t> = \
               Obj.magic({\"contents\": Js.null})"
              typ.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => input_%s.contents"
              typ.displayName typ.displayName);
         addWithNewLine
           (Printf.sprintf "let input_%s_conversionInstructions = [];"
              typ.displayName));

  (* Now add all of the conversion instructions. *)
  state.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
         addWithNewLine (printInputObjectAssets typ));

  (* Print the union type holders and getters *)
  state.unions
  |> Hashtbl.iter (fun _name (union : gqlUnion) ->
         addWithNewLine
           (Printf.sprintf
              "let union_%s: ref<GraphQLUnionType.t> = \
               Obj.magic({\"contents\": Js.null})"
              union.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => union_%s.contents"
              union.displayName union.displayName));

  addWithNewLine "";

  (* Print support functions for union type resolution *)
  state.unions
  |> Hashtbl.iter (fun _name (union : gqlUnion) ->
         addWithNewLine
           (Printf.sprintf
              "let union_%s_resolveType = (v: %s) => switch v {%s}\n"
              union.displayName
              (typeLocationToAccessor union.typeLocation)
              (union.types
              |> List.map (fun (member : gqlUnionMember) ->
                     Printf.sprintf " | %s(_) => get_%s()" member.displayName
                       member.displayName)
              |> String.concat "\n")));

  (* Now we can print all of the code that fills these in. *)
  state.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
         addWithNewLine
           (Printf.sprintf "t_%s.contents = GraphQLObjectType.make(%s)"
              typ.displayName
              (typ |> GenerateSchemaTypePrinters.printObjectType)));

  (* Now we can print all of the code that fills these in. *)
  state.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
         addWithNewLine
           (Printf.sprintf "input_%s.contents = GraphQLInputObjectType.make(%s)"
              typ.displayName
              (typ |> GenerateSchemaTypePrinters.printInputObjectType)));

  state.unions
  |> Hashtbl.iter (fun _name (union : gqlUnion) ->
         addWithNewLine
           (Printf.sprintf "union_%s.contents = GraphQLUnionType.make(%s)"
              union.displayName
              (union |> GenerateSchemaTypePrinters.printUnionType)));

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
         | Type ({kind = Record fields; attributes; decl}, _), Some ObjectType
           -> (
           let id = item.name in
           let displayName = capitalizeFirstChar item.name in

           let addedTyp =
             noticeObjectType ~env ~loc:decl.type_loc ~state:!state
               ?description:(attributesToDocstring attributes)
               ~makeFields:(fun () ->
                 fieldsOfRecordFields fields ~env ~full ~state:!state)
               id
           in
           match (addedTyp, displayName) with
           | Some typ, "Query" -> state := {!state with query = Some typ}
           | _ -> ())
         | Type ({kind = Record fields; attributes; decl}, _), Some InputObject
           ->
           let id = item.name in
           addInputObject id ~state:!state ~makeInputObject:(fun () ->
               {
                 id;
                 displayName = capitalizeFirstChar item.name;
                 fields =
                   inputObjectfieldsOfRecordFields fields ~env ~full
                     ~state:!state;
                 description = attributesToDocstring attributes;
                 typeLocation =
                   findTypeLocation item.name ~env ~loc:decl.type_loc
                     ~expectedType:InputObject;
               })
         | Type (({kind = Variant cases} as item), _), Some Enum ->
           addEnum item.name ~state:!state ~makeEnum:(fun () ->
               {
                 id = item.name;
                 displayName = capitalizeFirstChar item.name;
                 values = variantCasesToEnumValues cases;
                 description = item.attributes |> attributesToDocstring;
                 loc = item.decl.type_loc;
               })
         | Type (({kind = Variant cases} as item), _), Some Union ->
           addUnion item.name
             ~makeUnion:(fun () ->
               {
                 id = item.name;
                 displayName = capitalizeFirstChar item.name;
                 description = item.attributes |> attributesToDocstring;
                 types =
                   variantCasesToUnionValues cases ~env ~full ~state:!state;
                 typeLocation =
                   findTypeLocation ~loc:item.decl.type_loc ~env
                     ~expectedType:Union item.name;
               })
             ~state:!state
         | Value typ, Some Field -> (
           (* Values with a field annotation could be a resolver. *)
           match typ |> TypeUtils.extractType ~env ~package:full.package with
           | Some (Tfunction {typ; env}) -> (
             match extractResolverFunctionInfo ~env ~full ~state:!state typ with
             | Some (GraphQLObjectType {id}, args, returnType) ->
               let field =
                 {
                   loc = item.loc;
                   name = item.name;
                   description = item.attributes |> attributesToDocstring;
                   deprecationReason =
                     item.attributes
                     |> ProcessAttributes.findDeprecatedAttribute;
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
                                  isOptionLabelled =
                                    (match label with
                                    | Optional _ -> true
                                    | _ -> false);
                                  typ =
                                    getOrRaise
                                      (findGraphQLType ~full ~env ~state:!state
                                         typExpr)
                                      (Printf.sprintf
                                         "%s is wrong GraphQL type." name);
                                });
                 }
               in
               addFieldToObjectType ~env ~loc:item.loc ~field ~state:!state id
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
    let state =
      ref
        {
          types = Hashtbl.create 50;
          enums = Hashtbl.create 10;
          unions = Hashtbl.create 10;
          inputObjects = Hashtbl.create 10;
          query = None;
          diagnostics = [];
        }
    in
    traverseStructure structure ~state ~env ~full;
    if !state.diagnostics |> List.length > 0 then
      !state.diagnostics |> List.map printDiagnostic |> String.concat "\n\n"
      |> print_endline
    else
      let schemaCode = printSchemaJsFile !state |> formatCode in

      (* Write implementation file *)
      let oc = open_out outputPath in
      output_string oc schemaCode;
      close_out oc;

      (* Write resi file *)
      let oc = open_out (outputPath ^ "i") in
      output_string oc "let schema: ResGraph.schema";
      close_out oc;
      if debug then schemaCode |> print_endline
