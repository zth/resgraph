open GenerateSchemaTypes
open GenerateSchemaUtils

let variantCasesToEnumValues ~schemaState ~(env : SharedTypes.QueryEnv.t)
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | Args [] ->
           Some
             {
               value = case.cname.txt;
               description = case.attributes |> attributesToDocstring;
               deprecationReason = case.deprecated;
               loc = case.cname.loc;
             }
         | _ ->
           addDiagnostic schemaState
             ~diagnostic:
               {
                 loc = case.cname.loc;
                 message =
                   Printf.sprintf
                     "The variant member `%s` of the GraphQL enum variant `%s` \
                      has a payload. Variants tagged as @gql.enum can only \
                      have members without payloads. "
                     case.cname.txt (case.typeDecl |> fst);
                 fileUri = env.file.uri;
               };
           None)

type typeContext = Default | ReturnType

let rec lookupInterface ~env ~full ~schemaState path =
  let open SharedTypes in
  let resolved = ResolvePath.resolvePath ~env ~path ~package:full.package in
  match resolved with
  | None -> None
  | Some (env, p) ->
    traverseStructure ~schemaState ~originModule:env.file.moduleName ~env ~full
      env.file.structure;
    let interfaceIdentifier : gqlInterfaceIdentifier =
      {id = p; displayName = capitalizeFirstChar p}
    in
    Some interfaceIdentifier

(* Extracts valid GraphQL types from type exprs *)
and findGraphQLType ~env ?(typeContext = Default) ?loc ~schemaState
    ~(full : SharedTypes.full) (typ : Types.type_expr) =
  match typ.desc with
  | Tlink te | Tsubst te | Tpoly (te, []) ->
    findGraphQLType te ?loc ~env ~schemaState ~full ~typeContext
  | Tconstr (Path.Pident {name = "option"}, [unwrappedType], _) -> (
    let inner =
      findGraphQLType ~env ~schemaState ~full ~typeContext unwrappedType
    in
    match inner with
    | None -> None
    | Some inner -> Some (Nullable inner))
  | Tconstr (Path.Pident {name = "array"}, [unwrappedType], _) -> (
    let inner =
      findGraphQLType ?loc ~env ~schemaState ~full ~typeContext unwrappedType
    in
    match inner with
    | None -> None
    | Some inner -> Some (List inner))
  | Tconstr (Path.Pident {name = "promise"}, [unwrappedType], _)
    when typeContext = ReturnType ->
    findGraphQLType unwrappedType ?loc ~env ~schemaState ~full ~typeContext
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
        let inner =
          findGraphQLType ?loc ~env ~schemaState ~full ~typeContext typeArg
        in
        match inner with
        | None -> None
        | Some inner -> Some (RescriptNullable inner))
      | _ -> None)
    | _ -> (
      (* If none of the above matches we'll see if we can dig to the underlying
         type, to make sure it's a valid GraphQL type. *)
      match References.digConstructor ~env ~package:full.package path with
      | Some
          ( env,
            {item = {decl = {type_loc; type_params; type_manifest = Some te}}}
          ) ->
        (* Need to instantiate the type here, so all type variables are populated. *)
        let typeParams = type_params in
        let te = TypeUtils.instantiateType ~typeParams ~typeArgs te in
        findGraphQLType te ~loc:type_loc ~env ~schemaState ~full ~typeContext
      | Some (env, {item}) -> (
        let gqlAttribute =
          extractGqlAttribute ~env ~schemaState item.attributes
        in
        match (gqlAttribute, item) with
        | Some (ObjectType {interfaces}), {name; kind = Record fields} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          let interfaces =
            interfaces
            |> List.filter_map (fun interface ->
                   lookupInterface ~env ~full ~schemaState
                     (interface.Location.txt |> Utils.flattenLongIdent))
          in
          noticeObjectType id ~displayName ~schemaState ~env ~interfaces
            ~makeFields:(fun () ->
              fields |> objectTypeFieldsOfRecordFields ~env ~full ~schemaState)
            ~loc:item.decl.type_loc;
          Some (GraphQLObjectType {id; displayName})
        | Some InputObject, {name; kind = Record fields; attributes; decl} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addInputObject id ~schemaState ~makeInputObject:(fun () ->
              {
                id;
                displayName;
                fields =
                  inputObjectFieldsOfRecordFields fields ~env ~full ~schemaState;
                description = attributesToDocstring attributes;
                typeLocation =
                  findTypeLocation item.name ~env ~schemaState
                    ~loc:decl.type_loc ~expectedType:InputObject;
              });
          Some (GraphQLInputObject {id; displayName = capitalizeFirstChar id})
        | ( Some (Interface {interfaces}),
            {name; kind = Record fields; attributes; decl} ) ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          let interfaces =
            interfaces
            |> List.filter_map (fun interface ->
                   lookupInterface ~env ~full ~schemaState
                     (interface.Location.txt |> Utils.flattenLongIdent))
          in
          addInterface id ~schemaState ~makeInterface:(fun () ->
              {
                id;
                displayName;
                interfaces;
                fields =
                  objectTypeFieldsOfRecordFields fields ~env ~full ~schemaState;
                description = attributesToDocstring attributes;
                typeLocation =
                  findTypeLocation item.name ~env ~schemaState
                    ~loc:decl.type_loc ~expectedType:Interface;
              });
          Some (GraphQLInterface {id; displayName = capitalizeFirstChar id})
        | Some Enum, {name; kind = Variant cases} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addEnum id ~schemaState ~makeEnum:(fun () ->
              {
                id;
                displayName;
                values = variantCasesToEnumValues ~schemaState ~env cases;
                description = item.attributes |> attributesToDocstring;
                loc = item.decl.type_loc;
              });
          Some (GraphQLEnum {id; displayName = capitalizeFirstChar id})
        | Some Union, {name; kind = Variant cases} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addUnion id ~schemaState ~makeUnion:(fun () ->
              {
                id;
                displayName;
                types = variantCasesToUnionValues cases ~env ~full ~schemaState;
                description = item.attributes |> attributesToDocstring;
                typeLocation =
                  findTypeLocation ~loc:item.decl.type_loc ~schemaState ~env
                    ~expectedType:Union id;
              });
          Some (GraphQLUnion {id; displayName})
        | _ ->
          schemaState
          |> addDiagnostic
               ~diagnostic:
                 {
                   loc = item.decl.type_loc;
                   fileUri = env.file.uri;
                   message =
                     Printf.sprintf
                       "The type `%s` does not represent a valid GraphQL type."
                       item.name;
                 };
          None)
      | _ ->
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {
                 loc =
                   (match loc with
                   | None -> Location.in_file (env.file.moduleName ^ ".res")
                   | Some loc -> loc);
                 fileUri = env.file.uri;
                 message =
                   Printf.sprintf "This is not a valid GraphQL type: %s"
                     (Shared.typeToString typ);
               };
        None))
  | _ ->
    schemaState
    |> addDiagnostic
         ~diagnostic:
           {
             loc =
               (match loc with
               | None -> Location.in_file (env.file.moduleName ^ ".res")
               | Some loc -> loc);
             fileUri = env.file.uri;
             message =
               Printf.sprintf "This is not a valid GraphQL type: %s"
                 (Shared.typeToString typ);
           };
    None

and inputObjectFieldsOfRecordFields ~env ~schemaState ~(full : SharedTypes.full)
    (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         let name = field.fname.txt in
         match
           findGraphQLType field.typ ~loc:field.fname.loc ~full ~env
             ~schemaState
         with
         | None ->
           schemaState
           |> addDiagnostic
                ~diagnostic:
                  {
                    fileUri = env.file.uri;
                    loc = field.fname.loc;
                    message =
                      Printf.sprintf
                        "Field `%s` is is not a valid GraphQL type. All fields \
                         of a @gql.inputObject must be valid GraphQL types."
                        field.fname.txt;
                  };
           None
         | Some typ ->
           Some
             {
               name;
               resolverStyle = Property name;
               typ;
               args = [];
               description = field.attributes |> attributesToDocstring;
               deprecationReason = field.deprecated;
               loc = field.fname.loc;
             })

and variantCasesToUnionValues ~env ~schemaState ~full
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | Args [(typ, _)] -> (
           match
             findGraphQLType ~loc:case.cname.loc ~env ~schemaState ~full typ
           with
           | Some (GraphQLObjectType {id; displayName}) ->
             Some {objectTypeId = id; displayName; loc = case.cname.loc}
           | _ ->
             addDiagnostic schemaState
               ~diagnostic:
                 {
                   loc = case.cname.loc;
                   message =
                     Printf.sprintf
                       "The payload of the variant member `%s` of the GraphQL \
                        union variant `%s` is not a GraphL object. The payload \
                        needs to be a single type representing a GraphQL \
                        object, meaning it's annotated with @gql.type."
                       case.cname.txt (case.typeDecl |> fst);
                   fileUri = env.file.uri;
                 };
             None)
         | _ ->
           addDiagnostic schemaState
             ~diagnostic:
               {
                 loc = case.cname.loc;
                 message =
                   Printf.sprintf
                     "The payload of the variant member `%s` of the GraphQL \
                      union variant `%s` is not a single payload. The payload \
                      needs to be a single type representing a GraphQL object, \
                      meaning it's annotated with @gql.type."
                     case.cname.txt (case.typeDecl |> fst);
                 fileUri = env.file.uri;
               };
           None)

and objectTypeFieldsOfRecordFields ~env ~schemaState ~(full : SharedTypes.full)
    (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         match
           field.attributes
           |> getFieldAttributeFromRawAttributes ~schemaState ~env
         with
         | None -> None
         | Some attr -> Some (field, attr))
  |> List.filter_map (fun ((field : SharedTypes.field), _attr) ->
         let typ =
           findGraphQLType field.typ ~loc:field.fname.loc ~full ~env
             ~schemaState
         in
         match typ with
         | None ->
           schemaState
           |> addDiagnostic
                ~diagnostic:
                  {
                    fileUri = env.file.uri;
                    loc = field.fname.loc;
                    message =
                      Printf.sprintf
                        "Field `%s` is has a @gql.field annotation, but is not \
                         a valid GraphQL type. Please change the type of this \
                         field to be a valid GraphQL type, or expose this \
                         field via a custom written resolver instead if you \
                         need to transform it before exposing it in GraphQL."
                        field.fname.txt;
                  };
           None
         | Some typ ->
           let name = field.fname.txt in
           Some
             {
               name;
               resolverStyle = Property name;
               typ;
               args = [];
               description = field.attributes |> attributesToDocstring;
               deprecationReason = field.deprecated;
               loc = field.fname.loc;
             })

and extractResolverFunctionInfo ~env ?loc ~(full : SharedTypes.full)
    ~(schemaState : schemaState) (typ : Types.type_expr) =
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
        ( findGraphQLType typ ?loc ~env ~full ~schemaState,
          findGraphQLType returnType ~typeContext:ReturnType ?loc ~env ~full
            ~schemaState )
      with
      | Some targetGraphQLType, Some returnType ->
        Some (targetGraphQLType, args, returnType)
      | _ -> None)
    | _ -> None)
  | _ -> None

and mapFunctionArgs ~full ~env ~schemaState ~fnLoc
    (args : SharedTypes.typedFnArg list) =
  args
  |> List.filter_map (fun (label, typExpr) ->
         match findGraphQLType ~loc:fnLoc ~full ~env ~schemaState typExpr with
         | None ->
           schemaState
           |> addDiagnostic
                ~diagnostic:
                  {
                    fileUri = env.file.uri;
                    loc = fnLoc;
                    message =
                      Printf.sprintf
                        "Argument `%s` is is not a valid GraphQL type. All \
                         arguments of resolvers must be valid GraphQL types."
                        (match label with
                        | Asttypes.Nolabel -> "<unlabelled>"
                        | Labelled name | Optional name -> name);
                  };
           None
         | Some typ -> (
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
                 typ;
               }))

and traverseStructure ?(modulePath = []) ?originModule
    ~(schemaState : schemaState) ~env ~full
    (structure : SharedTypes.Module.structure) =
  if
    originModule |> Option.is_some
    && Hashtbl.mem schemaState.processedFiles (originModule |> Option.get)
  then ()
  else (
    if originModule |> Option.is_some then
      Hashtbl.replace schemaState.processedFiles
        (originModule |> Option.get)
        true;
    structure.items
    |> List.iter (fun (item : SharedTypes.Module.item) ->
           let gqlAttribute =
             item.attributes |> extractGqlAttribute ~schemaState ~env
           in
           match (item.kind, gqlAttribute) with
           | Module (Structure structure), _ ->
             (* Continue into modules (ignore module aliases etc) *)
             (* Might need to support modules with constraints too. But will
                ignore aliases. *)
             traverseStructure
               ~modulePath:(structure.name :: modulePath)
               ~schemaState ~env ~full structure
           | ( Type ({kind = Record fields; attributes; decl}, _),
               Some (ObjectType {interfaces}) ) ->
             (* @gql.type type someType = {...} *)
             let id = item.name in
             let displayName = capitalizeFirstChar item.name in
             let interfaces =
               interfaces
               |> List.filter_map (fun interface ->
                      lookupInterface ~env ~full ~schemaState
                        (interface.Location.txt |> Utils.flattenLongIdent))
             in
             (* TODO: Look up interfaces *)
             noticeObjectType ~env ~loc:decl.type_loc ~schemaState ~interfaces
               ?description:(attributesToDocstring attributes)
               ~displayName
               ~makeFields:(fun () ->
                 objectTypeFieldsOfRecordFields fields ~env ~full ~schemaState)
               id
           | ( Type ({kind = Record fields; attributes; decl}, _),
               Some InputObject ) ->
             (* @gql.inputObject type someInputObject = {...} *)
             let id = item.name in
             addInputObject id ~schemaState ~makeInputObject:(fun () ->
                 {
                   id;
                   displayName = capitalizeFirstChar item.name;
                   fields =
                     inputObjectFieldsOfRecordFields fields ~env ~full
                       ~schemaState;
                   description = attributesToDocstring attributes;
                   typeLocation =
                     findTypeLocation item.name ~env ~schemaState
                       ~loc:decl.type_loc ~expectedType:InputObject;
                 })
           | ( Type ({kind = Record fields; attributes; decl}, _),
               Some (Interface {interfaces}) ) ->
             (* @gql.interface type hasName = {...} *)
             let id = item.name in
             let interfaces =
               interfaces
               |> List.filter_map (fun interface ->
                      lookupInterface ~env ~full ~schemaState
                        (interface.Location.txt |> Utils.flattenLongIdent))
             in
             addInterface id ~schemaState ~makeInterface:(fun () ->
                 {
                   id;
                   displayName = capitalizeFirstChar item.name;
                   fields =
                     objectTypeFieldsOfRecordFields fields ~env ~full
                       ~schemaState;
                   description = attributesToDocstring attributes;
                   interfaces;
                   typeLocation =
                     findTypeLocation item.name ~env ~schemaState
                       ~loc:decl.type_loc ~expectedType:Interface;
                 })
           | Type (({kind = Variant cases} as item), _), Some Enum ->
             (* @gql.enum type someEnum = Online | Offline | Idle *)
             addEnum item.name ~schemaState ~makeEnum:(fun () ->
                 {
                   id = item.name;
                   displayName = capitalizeFirstChar item.name;
                   values = variantCasesToEnumValues ~schemaState ~env cases;
                   description = item.attributes |> attributesToDocstring;
                   loc = item.decl.type_loc;
                 })
           | Type (({kind = Variant cases} as item), _), Some Union ->
             (* @gql.union type userOrGroup = User(user) | Group(group) *)
             addUnion item.name
               ~makeUnion:(fun () ->
                 {
                   id = item.name;
                   displayName = capitalizeFirstChar item.name;
                   description = item.attributes |> attributesToDocstring;
                   types =
                     variantCasesToUnionValues cases ~env ~full ~schemaState;
                   typeLocation =
                     findTypeLocation ~loc:item.decl.type_loc ~env ~schemaState
                       ~expectedType:Union item.name;
                 })
               ~schemaState
           | Value typ, Some Field -> (
             (* @gql.field let fullName = (user: user) => {...} *)
             match typ |> TypeUtils.extractType ~env ~package:full.package with
             | Some (Tfunction {typ; env}) -> (
               match
                 extractResolverFunctionInfo ~loc:item.loc ~env ~full
                   ~schemaState typ
               with
               | Some (GraphQLObjectType {id}, args, returnType) ->
                 (* Resolver for object type. *)
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
                       mapFunctionArgs ~full ~env ~schemaState ~fnLoc:item.loc
                         args;
                   }
                 in
                 addFieldToObjectType ~env ~loc:item.loc ~field ~schemaState id
               | _ ->
                 schemaState
                 |> addDiagnostic
                      ~diagnostic:
                        {
                          loc = item.loc;
                          fileUri = env.file.uri;
                          message =
                            Printf.sprintf
                              "Could not figure out what GraphQL type to \
                               attach this resolver to. Make sure the first \
                               argument to your resolver is an unlabelled \
                               argument of the GraphQL type you want this \
                               resolver to be attached to.";
                        })
             | _ ->
               schemaState
               |> addDiagnostic
                    ~diagnostic:
                      {
                        loc = item.loc;
                        fileUri = env.file.uri;
                        message =
                          Printf.sprintf
                            "This let binding is annotated with @gql.field, \
                             but is not a function. Only functions can \
                             represent GraphQL field resolvers.";
                      })
           | _ -> (
             (* Didn't match. Do some error reporting. *)
             let baseDiagnostic =
               {fileUri = env.file.uri; message = ""; loc = item.loc}
             in
             let add = schemaState |> addDiagnostic in
             match gqlAttribute with
             | None -> (* Ignore if we had no gql attribute anyway. *) ()
             | Some Field ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This let binding is annotated with @gql.field, but \
                          is not a function. Only functions can represent \
                          GraphQL field resolvers.";
                   }
             | Some (ObjectType _) ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.type, but is not a \
                          record. Only records can represent GraphQL object \
                          types.";
                   }
             | Some InputObject ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.inputObject, but is \
                          not a record. Only records can represent GraphQL \
                          input objects.";
                   }
             | Some Enum ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.enum, but is not a \
                          variant. Only variants can represent GraphQL enums.";
                   }
             | Some Union ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.union, but is not a \
                          variant. Only variants can represent GraphQL unions.";
                   }
             | Some (Interface _) ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.interface, but is \
                          not a record. Only records can represent GraphQL \
                          interfaces.";
                   })))

let generateSchema ~path ~debug ~outputPath =
  if debug then Printf.printf "generating schema from %s\n\n" path;
  match Cmt.loadFullCmtFromPath ~path with
  | None -> ()
  | Some full ->
    let file = full.file in
    let structure = file.structure in
    let env = SharedTypes.QueryEnv.fromFile file in
    let schemaState =
      {
        types = Hashtbl.create 50;
        enums = Hashtbl.create 10;
        unions = Hashtbl.create 10;
        inputObjects = Hashtbl.create 10;
        interfaces = Hashtbl.create 10;
        query = None;
        subscription = None;
        mutation = None;
        diagnostics = [];
        processedFiles = Hashtbl.create 100;
      }
    in
    traverseStructure structure ~originModule:env.file.moduleName ~schemaState
      ~env ~full;
    if schemaState.diagnostics |> List.length > 0 then
      schemaState.diagnostics |> List.rev |> List.map printDiagnostic
      |> String.concat "\n\n" |> print_endline
    else
      let schemaCode =
        GenerateSchemaTypePrinters.printSchemaJsFile schemaState |> formatCode
      in

      (* Write implementation file *)
      let oc = open_out outputPath in
      output_string oc schemaCode;
      close_out oc;

      (* Write resi file *)
      let oc = open_out (outputPath ^ "i") in
      output_string oc "let schema: ResGraph.schema";
      close_out oc;

      if debug then schemaCode |> print_endline
