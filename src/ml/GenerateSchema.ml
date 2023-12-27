open GenerateSchemaTypes
open GenerateSchemaUtils
open GenerateSchemaDiagnostics

let variantCasesToEnumValues ~schemaState ~(env : SharedTypes.QueryEnv.t)
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | Args [] ->
           Some
             {
               value = nameFromAttribute case.attributes ~default:case.cname.txt;
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
                     "The variant case `%s` of the GraphQL enum variant `%s` \
                      has a payload. Variants tagged as @gql.enum can only \
                      have cases without payloads. "
                     case.cname.txt (case.typeDecl |> fst);
                 fileUri = env.file.uri;
               };
           None)

let rec extractFunctionType ~env ~package typ =
  let rec loop ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ~env acc t1
    | Tarrow (label, tArg, tRet, _) -> loop ~env ((label, tArg) :: acc) tRet
    | Tconstr (Pident {name = "function$"}, [t; _], _) ->
      extractFunctionType ~env ~package t
    | Tconstr (path, typeArgs, _) -> (
      match References.digConstructor ~env ~package path with
      | Some
          ( env,
            {
              item =
                {
                  decl =
                    {
                      type_attributes;
                      type_manifest = Some t1;
                      type_params = typeParams;
                    };
                };
            } ) ->
        (* Stop at types that have type annotations*)
        if GenerateSchemaUtils.hasGqlAnnotation type_attributes then
          (List.rev acc, t)
        else
          let t1 = t1 |> TypeUtils.instantiateType ~typeParams ~typeArgs in
          loop ~env acc t1
      | _ -> (List.rev acc, t))
    | _ -> (List.rev acc, t)
  in
  loop ~env [] typ

type typeContext = Default | ReturnType

let intfNameRegexp = Str.regexp "^Interface_\\(.*\\)$"
let extractInterfaceName str =
  if Str.string_match intfNameRegexp str 0 then Some (Str.matched_group 1 str)
  else None

(* Extracts valid GraphQL types from type exprs *)
let rec findGraphQLType ~(env : SharedTypes.QueryEnv.t) ?(typeContext = Default)
    ~debug ?loc ~schemaState ~(full : SharedTypes.full) (typ : Types.type_expr)
    =
  match typ.desc with
  | Tlink te | Tsubst te | Tpoly (te, []) ->
    findGraphQLType te ?loc ~env ~schemaState ~full ~debug ~typeContext
  | Tconstr (Path.Pident {name = "option"}, [unwrappedType], _) -> (
    let inner =
      findGraphQLType ~env ~schemaState ~debug ~full ~typeContext unwrappedType
    in
    match inner with
    | None -> None
    | Some inner -> Some (Nullable inner))
  | Tconstr (Path.Pident {name = "array"}, [unwrappedType], _) -> (
    let inner =
      findGraphQLType ?loc ~env ~schemaState ~debug ~full ~typeContext
        unwrappedType
    in
    match inner with
    | None -> None
    | Some inner -> Some (List inner))
  | Tconstr (Path.Pident {name = "promise"}, [unwrappedType], _)
    when typeContext = ReturnType ->
    findGraphQLType unwrappedType ?loc ~env ~debug ~schemaState ~full
      ~typeContext
  | Tconstr (Path.Pident {name = "string"}, [], _) -> Some (Scalar String)
  | Tconstr (Path.Pident {name = "bool"}, [], _) -> Some (Scalar Boolean)
  | Tconstr (Path.Pident {name = "int"}, [], _) -> Some (Scalar Int)
  | Tconstr (Path.Pident {name = "float"}, [], _) -> Some (Scalar Float)
  | Tconstr (path, typeArgs, _) -> (
    match pathIdentToList path with
    | [intfFilename; "ImplementedBy"; "t"]
      when Utils.startsWith intfFilename "Interface_" -> (
      let interfaceName = extractInterfaceName intfFilename in
      match interfaceName with
      | None -> None
      | Some interfaceName -> Some (InjectInterfaceTypename interfaceName))
    | ["ResGraph"; "id"] -> Some (Scalar ID)
    | ["ResGraphContext"; "context"] -> Some InjectContext
    | ["Js"; "Nullable"; "t"] -> (
      match typeArgs with
      | [typeArg] -> (
        let inner =
          findGraphQLType ?loc ~env ~schemaState ~debug ~full ~typeContext
            typeArg
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
            {
              item =
                {
                  decl =
                    {
                      type_loc;
                      type_params;
                      type_manifest = Some te;
                      type_attributes;
                    };
                };
            } )
      (* Follow type aliases only if this isn't a defined GraphQL type. *)
        when type_attributes |> hasGqlAnnotation = false ->
        (* Need to instantiate the type here, so all type variables are populated. *)
        let typeParams = type_params in
        let te = TypeUtils.instantiateType ~typeParams ~typeArgs te in
        findGraphQLType te ~loc:type_loc ~env ~debug ~schemaState ~full
          ~typeContext
      | Some (env, {item}) -> (
        let gqlAttribute =
          extractGqlAttribute ~env ~schemaState item.attributes
        in
        match (gqlAttribute, item) with
        | ( Some ObjectType,
            {
              attributes;
              name = ("query" | "mutation") as name;
              kind = Abstract None;
            } ) ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          noticeObjectType id ~displayName ~schemaState ~env
            ?description:(GenerateSchemaUtils.attributesToDocstring attributes)
            ~makeFields:(fun () -> [])
            ~loc:item.decl.type_loc;
          Some (GraphQLObjectType {id; displayName})
        | Some ObjectType, {attributes; name; kind = Record fields} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          noticeObjectType id ~displayName ~schemaState ~env
            ?description:(GenerateSchemaUtils.attributesToDocstring attributes)
            ~makeFields:(fun () ->
              fields
              |> objectTypeFieldsOfRecordFields ~env ~debug ~full ~schemaState)
            ~loc:item.decl.type_loc;
          Some (GraphQLObjectType {id; displayName})
        | ( Some ObjectType,
            {attributes; name; kind = Abstract (Some (_p, typeArgs))} )
          when List.length typeArgs > 0 ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          noticeObjectType id ~displayName ~schemaState ~env
            ?description:(GenerateSchemaUtils.attributesToDocstring attributes)
            ~makeFields:(fun () -> [])
            ~loc:item.decl.type_loc;
          Some (GraphQLObjectType {id; displayName})
        | Some InputObject, {name; kind = Record fields; attributes; decl} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addInputObject id ~schemaState ~debug ~makeInputObject:(fun () ->
              {
                id;
                displayName;
                fields =
                  inputObjectFieldsOfRecordFields fields ~env ~debug ~full
                    ~schemaState;
                description = attributesToDocstring attributes;
                syntheticTypeLocation = None;
                typeLocation =
                  Some
                    (findTypeLocation item.name ~env ~schemaState
                       ~loc:decl.type_loc ~expectedType:InputObject);
              });
          Some (GraphQLInputObject {id; displayName = capitalizeFirstChar id})
        | Some Interface, {name; kind = Record fields; attributes; decl} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addInterface id ~schemaState ~debug ~makeInterface:(fun () ->
              {
                id;
                displayName;
                interfaces = [];
                fields =
                  objectTypeFieldsOfRecordFields fields ~env ~full ~schemaState
                    ~debug;
                description = attributesToDocstring attributes;
                typeLocation =
                  findTypeLocation item.name ~env ~schemaState
                    ~loc:decl.type_loc ~expectedType:Interface;
              });
          Some (GraphQLInterface {id; displayName = capitalizeFirstChar id})
        | Some Enum, {name; kind = Variant cases} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addEnum id ~schemaState ~debug ~makeEnum:(fun () ->
              {
                id;
                displayName;
                values = variantCasesToEnumValues ~schemaState ~env cases;
                description = item.attributes |> attributesToDocstring;
                typeLocation =
                  findTypeLocation ~loc:item.decl.type_loc ~schemaState ~env
                    ~expectedType:Enum id;
              });
          Some (GraphQLEnum {id; displayName = capitalizeFirstChar id})
        | Some Union, {name; kind = Variant cases} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addUnion id ~schemaState ~debug ~makeUnion:(fun () ->
              {
                id;
                displayName;
                types =
                  variantCasesToUnionValues cases ~env ~full ~schemaState ~debug
                    ~ownerName:displayName;
                description = item.attributes |> attributesToDocstring;
                typeLocation =
                  findTypeLocation ~loc:item.decl.type_loc ~schemaState ~env
                    ~expectedType:Union id;
              });
          Some (GraphQLUnion {id; displayName})
        | Some InputUnion, {name; kind = Variant cases} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addInputUnion id ~schemaState ~debug ~makeInputUnion:(fun () ->
              {
                id;
                displayName;
                members =
                  variantCasesToInputUnionValues cases ~env ~full ~schemaState
                    ~debug ~ownerName:displayName;
                description = item.attributes |> attributesToDocstring;
                typeLocation =
                  findTypeLocation ~loc:item.decl.type_loc ~schemaState ~env
                    ~expectedType:InputUnion id;
              });
          Some
            (GraphQLInputUnion
               {
                 id;
                 displayName;
                 inlineRecords =
                   cases
                   |> List.filter_map (fun (c : SharedTypes.Constructor.t) ->
                          match c.args with
                          | InlineRecord _ -> Some c.cname.txt
                          | _ -> None);
               })
        | Some (InterfaceResolver {interfaceId}), {kind = Variant _} ->
          Some
            (GraphQLInterface
               {id = interfaceId; displayName = capitalizeFirstChar interfaceId})
        | Some Scalar, {name = "t"} ->
          (* When the type is named `t`, we assume the module name is the scalar
             name. And the module name is the last added entry in pathRev, which
             tracks the current path we're at in the module of env. *)
          let scalarName = env.pathRev |> List.hd in
          Some (GraphQLScalar {id = scalarName; displayName = scalarName})
        | Some Scalar, {name; kind = Abstract (Some _)} ->
          Some
            (GraphQLScalar {id = name; displayName = capitalizeFirstChar name})
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
      | None ->
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

and inputObjectFieldsOfRecordFields ~env ~debug ~schemaState
    ~(full : SharedTypes.full) (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         let name = field.fname.txt in
         match
           findGraphQLType field.typ ~debug ~loc:field.fname.loc ~full ~env
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
               fileName = env.file.moduleName;
               fileUri = env.file.uri;
               args = [];
               description = field.attributes |> attributesToDocstring;
               deprecationReason = field.deprecated;
               loc = field.fname.loc;
               onType = None;
             })

and variantCasesToUnionValues ~env ~debug ~schemaState ~full ~ownerName
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | InlineRecord fields ->
           let syntheticTypeName = ownerName ^ case.cname.txt in
           let id = syntheticTypeName in
           noticeObjectType id ~displayName:syntheticTypeName
             ~ignoreTypeLocation:true
             ~syntheticTypeLocation:
               {fileUri = env.file.uri; loc = case.cname.loc}
             ~schemaState ~env
             ~makeFields:(fun () ->
               fields
               |> objectTypeFieldsOfInlineRecordFields ~env ~full ~schemaState
                    ~debug)
             ~loc:case.cname.loc;
           let member : gqlUnionMember =
             {
               objectTypeId = id;
               displayName = syntheticTypeName;
               loc = case.cname.loc;
               description =
                 case.attributes |> ProcessAttributes.findDocAttribute;
               constructorName = case.cname.txt;
             }
           in
           Some member
         | Args [(typ, _)] -> (
           match
             findGraphQLType ~debug ~loc:case.cname.loc ~env ~schemaState ~full
               typ
           with
           | Some (GraphQLObjectType {id; displayName}) ->
             Some
               {
                 objectTypeId = id;
                 displayName;
                 loc = case.cname.loc;
                 description =
                   case.attributes |> ProcessAttributes.findDocAttribute;
                 constructorName = case.cname.txt;
               }
           | _ ->
             addDiagnostic schemaState
               ~diagnostic:
                 {
                   loc = case.cname.loc;
                   message =
                     Printf.sprintf
                       "The payload of the variant case `%s` of the GraphQL \
                        union variant `%s` is not a GraphQL object. The \
                        payload needs to be a single type representing a \
                        GraphQL object, meaning it's annotated with @gql.type."
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
                     "The payload of the variant case `%s` of the GraphQL \
                      union variant `%s` is not a single payload. The payload \
                      needs to be a single type representing a GraphQL object, \
                      meaning it's annotated with @gql.type."
                     case.cname.txt (case.typeDecl |> fst);
                 fileUri = env.file.uri;
               };
           None)

and variantCasesToInputUnionValues ~env ~debug ~schemaState ~full ~ownerName
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | InlineRecord fields ->
           let syntheticTypeName = ownerName ^ case.cname.txt in
           let id = syntheticTypeName in
           let displayName = capitalizeFirstChar id in
           addInputObject id ~schemaState ~debug ~makeInputObject:(fun () ->
               {
                 id;
                 displayName = capitalizeFirstChar id;
                 fields =
                   fields
                   |> objectTypeFieldsOfInlineRecordFields ~env ~full
                        ~schemaState ~debug;
                 description = None;
                 typeLocation = None;
                 syntheticTypeLocation =
                   Some {fileUri = env.file.uri; loc = case.cname.loc};
               });
           let member : gqlInputUnionMember =
             {
               typ = GraphQLInputObject {displayName; id};
               fieldName = uncapitalizeFirstChar case.cname.txt;
               loc = case.cname.loc;
               description =
                 case.attributes |> ProcessAttributes.findDocAttribute;
               constructorName = case.cname.txt;
             }
           in
           Some member
         | Args [(typ, _)] -> (
           match
             findGraphQLType ~debug ~loc:case.cname.loc ~env ~schemaState ~full
               typ
           with
           | Some typ ->
             Some
               {
                 typ;
                 fieldName = uncapitalizeFirstChar case.cname.txt;
                 loc = case.cname.loc;
                 description =
                   case.attributes |> ProcessAttributes.findDocAttribute;
                 constructorName = case.cname.txt;
               }
           | _ ->
             addDiagnostic schemaState
               ~diagnostic:
                 {
                   loc = case.cname.loc;
                   message =
                     Printf.sprintf
                       "The payload of the variant case `%s` of the GraphQL \
                        input union variant `%s` is not a GraphQL input \
                        object. The payload needs to be a single type \
                        representing a GraphQL input object, meaning it's \
                        annotated with @gql.inputObject."
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
                     "The payload of the variant case `%s` of the GraphQL \
                      input union variant `%s` is not a single payload. The \
                      payload needs to be a single type representing a GraphQL \
                      input object, meaning it's annotated with \
                      @gql.inputObject."
                     case.cname.txt (case.typeDecl |> fst);
                 fileUri = env.file.uri;
               };
           None)

and objectTypeFieldsOfRecordFields ~env ~schemaState ~debug
    ~(full : SharedTypes.full) (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         match
           field.attributes
           |> getFieldAttributeFromRawAttributes ~schemaState ~env
         with
         | None -> None
         | Some attr -> Some (field, attr))
  |> List.filter_map (fun ((field : SharedTypes.field), _attr) ->
         let fieldType = field.typ in
         let typ =
           findGraphQLType fieldType ~debug ~loc:field.fname.loc ~full ~env
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
               fileName = env.file.moduleName;
               fileUri = env.file.uri;
               args = [];
               description = field.attributes |> attributesToDocstring;
               deprecationReason = field.deprecated;
               loc = field.fname.loc;
               onType = None;
             })

and objectTypeFieldsOfInlineRecordFields ~env ~schemaState ~debug
    ~(full : SharedTypes.full) (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         let fieldType = field.typ in
         let typ =
           findGraphQLType fieldType ~debug ~loc:field.fname.loc ~full ~env
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
                        "Field `%s` is not a valid GraphQL type. Please change \
                         the type of this field to be a valid GraphQL type, or \
                         expose this field via a custom written resolver \
                         instead if you need to transform it before exposing \
                         it in GraphQL."
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
               fileName = env.file.moduleName;
               fileUri = env.file.uri;
               args = [];
               description = field.attributes |> attributesToDocstring;
               deprecationReason = field.deprecated;
               loc = field.fname.loc;
               onType = None;
             })

and extractResolverFunctionInfo ~resolverName ~env ?loc
    ~(full : SharedTypes.full) ~(schemaState : schemaState) ~debug
    (typ : Types.type_expr) =
  let args, returnType = extractFunctionType ~env ~package:full.package typ in
  (* The first arg should point to the type this resolver is for. *)
  match List.nth_opt args 0 with
  | Some (Nolabel, typ) -> (
    if debug then
      Printf.printf "Checking for type to attach resolver %s to\n" resolverName;
    match
      ( findGraphQLType typ ~debug ?loc ~env ~full ~schemaState,
        findGraphQLType returnType ~debug ~typeContext:ReturnType ?loc ~env
          ~full ~schemaState )
    with
    | Some targetGraphQLType, Some returnType ->
      Some (targetGraphQLType, args, returnType)
    | _ -> None)
  | _ -> None

and mapFunctionArgs ~full ~env ~debug ~schemaState ~fnLoc
    (args : SharedTypes.typedFnArg list) =
  args
  |> List.filter_map (fun (label, typExpr) ->
         match
           findGraphQLType ~debug ~loc:fnLoc ~full ~env ~schemaState typExpr
         with
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

and traverseStructure ?(modulePath = []) ?implStructure ?originModule
    ~(schemaState : schemaState) ~env ~debug ~(full : SharedTypes.full)
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
               ~schemaState ~env ~full ~debug structure
           | ( Module
                 (Constraint (Structure implStructure, Structure intfStructure)),
               _ ) ->
             (* Look inside constraints rather than structure when present. *)
             traverseStructure ~implStructure
               ~modulePath:(intfStructure.name :: modulePath)
               ~schemaState ~env ~full ~debug intfStructure
           | ( Type
                 ( {
                     name = "query" | "mutation";
                     kind = Abstract None;
                     attributes;
                     decl;
                   },
                   _ ),
               Some ObjectType ) ->
             (* @gql.type type query *)
             (* @gql.type type mutation *)
             let id = item.name in
             let displayName = capitalizeFirstChar item.name in
             noticeObjectType ~env ~loc:decl.type_loc ~schemaState
               ?description:(attributesToDocstring attributes)
               ~displayName
               ~makeFields:(fun () -> [])
               id
           | Type ({kind = Record fields; attributes; decl}, _), Some ObjectType
             ->
             (* @gql.type type someType = {...} *)
             let id = item.name in
             let displayName = capitalizeFirstChar item.name in
             noticeObjectType ~env ~loc:decl.type_loc ~schemaState
               ?description:(attributesToDocstring attributes)
               ~displayName
               ~makeFields:(fun () ->
                 objectTypeFieldsOfRecordFields fields ~env ~full ~schemaState
                   ~debug)
               id
           | ( Type ({kind = Record fields; attributes; decl}, _),
               Some InputObject ) ->
             (* @gql.inputObject type someInputObject = {...} *)
             let id = item.name in
             addInputObject id ~schemaState ~debug ~makeInputObject:(fun () ->
                 {
                   id;
                   displayName = capitalizeFirstChar item.name;
                   fields =
                     inputObjectFieldsOfRecordFields fields ~env ~full
                       ~schemaState ~debug;
                   description = attributesToDocstring attributes;
                   syntheticTypeLocation = None;
                   typeLocation =
                     Some
                       (findTypeLocation item.name ~env ~schemaState
                          ~loc:decl.type_loc ~expectedType:InputObject);
                 })
           | Type ({kind = Record fields; attributes; decl}, _), Some Interface
             ->
             (* @gql.interface type hasName = {...} *)
             let id = item.name in
             addInterface id ~schemaState ~debug ~makeInterface:(fun () ->
                 {
                   id;
                   displayName = capitalizeFirstChar item.name;
                   fields =
                     objectTypeFieldsOfRecordFields fields ~env ~full
                       ~schemaState ~debug;
                   description = attributesToDocstring attributes;
                   interfaces = [];
                   typeLocation =
                     findTypeLocation item.name ~env ~schemaState
                       ~loc:decl.type_loc ~expectedType:Interface;
                 })
           | Type (({kind = Variant cases} as item), _), Some Enum ->
             (* @gql.enum type someEnum = Online | Offline | Idle *)
             (* TODO: Can inline all type locs *)
             addEnum item.name ~schemaState ~debug ~makeEnum:(fun () ->
                 {
                   id = item.name;
                   displayName = capitalizeFirstChar item.name;
                   values = variantCasesToEnumValues ~schemaState ~env cases;
                   description = item.attributes |> attributesToDocstring;
                   typeLocation =
                     findTypeLocation ~loc:item.decl.type_loc ~schemaState ~env
                       ~expectedType:Enum item.name;
                 })
           | Type (({kind = Variant cases} as item), _), Some Union ->
             (* @gql.union type userOrGroup = User(user) | Group(group) *)
             let displayName = capitalizeFirstChar item.name in
             addUnion item.name
               ~makeUnion:(fun () ->
                 {
                   id = item.name;
                   displayName;
                   description = item.attributes |> attributesToDocstring;
                   types =
                     variantCasesToUnionValues cases ~env ~full ~schemaState
                       ~debug ~ownerName:displayName;
                   typeLocation =
                     findTypeLocation ~loc:item.decl.type_loc ~env ~schemaState
                       ~expectedType:Union item.name;
                 })
               ~schemaState ~debug
           | Type (({kind = Variant cases} as item), _), Some InputUnion ->
             (* @gql.inputUnion type location = Coordinates(coordinates) | Address(address) *)
             let displayName = capitalizeFirstChar item.name in
             addInputUnion item.name
               ~makeInputUnion:(fun () ->
                 {
                   id = item.name;
                   displayName;
                   description = item.attributes |> attributesToDocstring;
                   members =
                     variantCasesToInputUnionValues cases ~env ~full
                       ~schemaState ~debug ~ownerName:displayName;
                   typeLocation =
                     findTypeLocation ~loc:item.decl.type_loc ~env ~schemaState
                       ~expectedType:InputUnion item.name;
                 })
               ~schemaState ~debug
           | Type (({name = "t"} as item), _), Some Scalar -> (
             (* module Timestamp = { @gql.scalar type t = string } *)
             (* module Timestamp: {@gql.scalar type t } = { type t = string } *)
             let typeName = modulePath |> List.hd in
             let typeLoc =
               {
                 fileName = env.file.moduleName;
                 modulePath = List.rev modulePath;
                 typeName = "t";
                 loc = item.decl.type_loc;
                 fileUri = env.file.uri;
               }
             in

             let hasParseValueFn =
               structure.items
               |> List.exists (fun (i : SharedTypes.Module.item) ->
                      match i with
                      | {name = "parseValue"; kind = Value _} -> true
                      | _ -> false)
             in
             (* We just check for parse and serialize functions existance here,
                and let the typesystem in the project validate that they work
                together with the scalar type. We might need to adjust this to
                be a ResGraph validation if it gets problematic.*)
             let hasSerializeFn =
               structure.items
               |> List.exists (fun (i : SharedTypes.Module.item) ->
                      match i with
                      | {name = "serialize"; kind = Value _} -> true
                      | _ -> false)
             in
             (* Look up the implemented type if it's behind an interface. *)
             let implementedType =
               match implStructure with
               | None -> item
               | Some {items} ->
                 items
                 |> List.find_map (fun (itm : SharedTypes.Module.item) ->
                        match itm with
                        | {name = "t"; kind = Type (tt, _)} -> Some tt
                        | _ -> None)
                 |> Option.value ~default:item
             in

             (* Check whether the implemented type needs parsing *)
             let needsParsing =
               match implementedType with
               | {kind = Abstract (Some (path, typs))} ->
                 let asTypExpr = Ctype.newconstr path typs in
                 validateCustomScalar ~env ~package:full.package asTypExpr
                 = NeedsParsing
               | _ -> true
             in
             match (needsParsing, hasParseValueFn, hasSerializeFn) with
             | (true | false), true, true ->
               (* Has parsers, always add them *)
               addScalar typeName
                 ?description:(item.attributes |> attributesToDocstring)
                 ~encoderDecoderLoc:typeLoc ~typeLocation:typeLoc ~schemaState
                 ~debug
             | true, false, true | true, true, false | true, false, false ->
               (* Needs parsing, but missing one of the assets *)
               schemaState
               |> addDiagnostic
                    ~diagnostic:
                      {
                        loc = item.decl.type_loc;
                        fileUri = env.file.uri;
                        message =
                          Printf.sprintf
                            "Scalars named `t` in a module where the \
                             underlying implementation is not serializable to \
                             JSON needs accompanying `parseValue` and \
                             `serialize` functions for serializing and parsing \
                             values for the scalar. One or both of those \
                             functions is not defined in this module.\n\n\
                             Either define them, or change the implementation \
                             of `t` to use a type that's serializable to JSON \
                             without conversion.";
                      }
             | false, _, _ ->
               (* Does not need parsing, and don't have assets *)
               addScalar typeName
                 ?description:(item.attributes |> attributesToDocstring)
                 ~typeLocation:typeLoc ~schemaState ~debug)
           | ( Type (({kind = Abstract (Some (path, typs))} as item), _),
               Some Scalar ) -> (
             (* @gql.scalar type someScalar = string *)
             let asTypExpr = Ctype.newconstr path typs in
             match
               validateCustomScalar ~env ~package:full.package asTypExpr
             with
             | DoesNotNeedParsing ->
               addScalar item.name
                 ?description:(item.attributes |> attributesToDocstring)
                 ~typeLocation:
                   (findTypeLocation ~loc:item.decl.type_loc ~env ~schemaState
                      ~expectedType:Scalar item.name)
                 ~schemaState ~debug
             | NeedsParsing ->
               (* TODO: More docs *)
               schemaState
               |> addDiagnostic
                    ~diagnostic:
                      {
                        loc = item.decl.type_loc;
                        fileUri = env.file.uri;
                        message =
                          Printf.sprintf
                            "This GraphQL scalar maps to something that \
                             requires custom parsing and serializing. You can \
                             either change this to a valid JSON type, or you \
                             can create a module that defines a `type t` \
                             annotated with `@gql.scalar`, and a `parseValue` \
                             and `serialize` function. Better explanation and \
                             docs coming soon.";
                      })
           | Value typ, Some Field -> (
             (* @gql.field let fullName = (user: user) => {...} *)
             match
               extractResolverFunctionInfo ~resolverName:item.name ~loc:item.loc
                 ~env ~full ~schemaState ~debug typ
             with
             | Some (GraphQLObjectType {id}, args, returnType) ->
               (* Resolver for object type. *)
               let args =
                 mapFunctionArgs ~full ~debug ~env ~schemaState ~fnLoc:item.loc
                   args
               in
               (* Validate that inject intf typename arg is not present here, as
                  it's only valid in interface fns. *)
               (match
                  args
                  |> List.find_opt (fun (arg : gqlArg) ->
                         match arg.typ with
                         | InjectInterfaceTypename _ -> true
                         | _ -> false)
                with
               | None -> ()
               | Some {name} ->
                 schemaState
                 |> addDiagnostic
                      ~diagnostic:
                        {
                          loc = item.loc;
                          fileUri = env.file.uri;
                          message =
                            Printf.sprintf
                              "Argument \"%s\" is trying to inject a interface \
                               typename, that's however only valid for field \
                               functions adding fields to interfaces."
                              name;
                        });
               let field =
                 {
                   loc = item.loc;
                   name = item.name;
                   fileName = env.file.moduleName;
                   fileUri = env.file.uri;
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
                   args;
                   onType = None;
                 }
               in
               addFieldToObjectType ~env ~loc:item.loc ~field ~schemaState id
             | Some (GraphQLInterface {id}, args, returnType) ->
               (* Resolver for interface type. *)
               let args =
                 mapFunctionArgs ~full ~debug ~env ~schemaState ~fnLoc:item.loc
                   args
               in
               (* Validate interface typename injection if present. *)
               (match
                  args
                  |> List.find_map (fun (arg : gqlArg) ->
                         match arg.typ with
                         | InjectInterfaceTypename targetIntfId ->
                           Some (targetIntfId, arg)
                         | _ -> None)
                with
               | Some (targetIntfId, arg) when targetIntfId <> id ->
                 schemaState
                 |> addDiagnostic
                      ~diagnostic:
                        {
                          loc = item.loc;
                          fileUri = env.file.uri;
                          message =
                            Printf.sprintf
                              "Argument \"%s\" is trying to inject a interface \
                               typename, but it's targeting the wrong \
                               interface (\"%s\" vs wanted \"%s\"). Please \
                               change the type annotation to \
                               \"Interface_%s.ImplementedBy.t\"."
                              arg.name targetIntfId id id;
                        }
               | _ -> ());
               let field =
                 {
                   loc = item.loc;
                   name = item.name;
                   fileName = env.file.moduleName;
                   fileUri = env.file.uri;
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
                   args;
                   onType = None;
                 }
               in
               addFieldToInterfaceType ~env ~loc:item.loc ~field ~schemaState id
             | _ ->
               schemaState
               |> addDiagnostic
                    ~diagnostic:
                      {
                        loc = item.loc;
                        fileUri = env.file.uri;
                        message =
                          Printf.sprintf
                            "Could not figure out what GraphQL type to attach \
                             this resolver to. Make sure the first argument to \
                             your resolver is an unlabelled argument of the \
                             GraphQL type you want this resolver to be \
                             attached to.";
                      })
           | _, None ->
             (* Ignore values with no gql attribute. *)
             ()
           | _, Some (InterfaceResolver _) ->
             (* Ignore interface resolvers, they're handled elsewhere *)
             ()
           | _, Some _ -> (
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
             | Some ObjectType ->
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
             | Some InputUnion ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.inputUnion, but is \
                          not a variant. Only variants can represent GraphQL \
                          input union.";
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
             | Some Scalar ->
               (* TODO: More docs on custom scalars. *)
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.scalar, but is not \
                          a type alias. Only type aliases can represent \
                          GraphQL scalars as of now (but other representations \
                          will be possible in the future).";
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
             | Some Interface ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.interface, but is \
                          not a record. Only records can represent GraphQL \
                          interfaces.";
                   }
             | Some (InterfaceResolver _) ->
               add
                 ~diagnostic:
                   {
                     baseDiagnostic with
                     message =
                       Printf.sprintf
                         "This type is annotated with @gql.interfaceResolver, \
                          but is not a variant. Only variants can represent \
                          GraphQL interface resolvers.";
                   })))

let printErrorState diagnostics =
  Printf.printf
    "{\n  \"status\": \"Error\",\n  \"errors\": \n    [\n      %s\n    ]\n}"
    (diagnostics |> List.rev |> List.map printDiagnostic |> String.concat ",\n")

let generateSchema ~printToStdOut ~writeStateFile ~sourceFolder ~debug
    ~outputFolder ~writeSdlFile =
  if debug then Printf.printf "generating schema from %s\n\n" sourceFolder;
  (* Holds cmt cache so we don't do heavy cmt processing multiple times. *)
  let cmtCache = Hashtbl.create 100 in
  let fileHasGqlAttributeCache = Hashtbl.create 100 in
  let schemaState =
    {
      types = Hashtbl.create 50;
      enums = Hashtbl.create 10;
      unions = Hashtbl.create 10;
      inputObjects = Hashtbl.create 10;
      inputUnions = Hashtbl.create 10;
      interfaces = Hashtbl.create 10;
      scalars = Hashtbl.create 10;
      query = None;
      subscription = None;
      mutation = None;
      diagnostics = [];
      processedFiles = Hashtbl.create 100;
    }
  in
  let processFileAtPath path =
    let fileAssets =
      match Hashtbl.find_opt cmtCache path with
      | None ->
        let sourceHasGqlAttr =
          match Hashtbl.find_opt fileHasGqlAttributeCache path with
          | None ->
            let hasAttr = GenerateSchemaUtils.fileHasGqlAttribute path in
            Hashtbl.replace fileHasGqlAttributeCache path hasAttr;
            hasAttr
          | Some v -> v
        in
        if sourceHasGqlAttr then (
          match Cmt.loadFullCmtFromPath ~path with
          | None -> None
          | Some full ->
            let file = full.file in
            let env = SharedTypes.QueryEnv.fromFile file in
            let res = (full, env) in
            Hashtbl.replace cmtCache path res;
            Some res)
        else None
      | v -> v
    in
    match fileAssets with
    | None -> None
    | Some (full, env) ->
      traverseStructure full.file.structure ~originModule:env.file.moduleName
        ~schemaState ~env ~full ~debug;
      Some full
  in

  let package =
    match Packages.getPackage ~uri:(Uri.fromPath sourceFolder) with
    | None ->
      printErrorState
        [
          {
            loc = emptyLoc;
            fileUri = Uri.fromPath sourceFolder;
            message =
              Printf.sprintf
                "Source folder \"%s\" is not in a ReScript project. Did you \
                 configure this correctly?"
                sourceFolder;
          };
        ];
      exit 1
    | Some files -> files
  in

  package.projectFiles
  |> SharedTypes.FileSet.iter (fun file ->
         (* TODO: This file filter contains a bunch of things I'm sure won't
            be necessary when this is a real package. Keep it for now to make
            tests work. *)
         match file with
         | "ResGraph" | "ResGraphSchema" | "GraphQLYoga" | "Errors"
         | "ResGraph__GraphQLJs" ->
           ()
         | _ -> (
           match Hashtbl.find package.pathsForModule file with
           | IntfAndImpl {res} | Impl {res} -> processFileAtPath res |> ignore
           | _ -> ()));

  let processedSchema = processSchema schemaState in
  let schemaOutputPath = outputFolder ^ "/ResGraphSchema.res" in
  let sdlOutputPath = outputFolder ^ "/schema.graphql" in

  if schemaState.diagnostics |> List.length > 0 then (
    if printToStdOut then
      Printf.printf
        "{\n  \"status\": \"Error\",\n  \"errors\": \n    [\n      %s\n    ]\n}"
        (schemaState.diagnostics |> List.rev
        |> List.map (fun (_, diagnostic) -> printDiagnostic diagnostic)
        |> String.concat ",\n");

    (* Write an empty schema just to avoid type errors in the generated code. *)
    GenerateSchemaUtils.writeIfHasChanges schemaOutputPath
      "let schema = ResGraph__GraphQLJs.GraphQLSchemaType.make(Obj.magic())\n")
  else
    let schemaCode =
      GenerateSchemaTypePrinters.printSchemaJsFile schemaState processedSchema
      |> formatCode ~debug
    in

    GenerateSchemaTypePrinters.cleanInterfaceFiles schemaState ~outputFolder;
    GenerateSchemaTypePrinters.printInterfaceFiles schemaState ~processedSchema
      ~outputFolder ~debug;

    (* TODO: Do this in parallell in some fancy way *)
    if writeStateFile then
      GenerateSchemaUtils.writeStateFile ~package ~schemaState ~processedSchema;

    (if writeSdlFile then
       let sdl = GenerateSchemaSDL.printSchemaSDL schemaState in
       GenerateSchemaUtils.writeIfHasChanges sdlOutputPath sdl);

    (* Write generated schema *)
    (* Write implementation file *)
    GenerateSchemaUtils.writeIfHasChanges schemaOutputPath schemaCode;

    (* Write resi file *)
    let resiOutputPath = schemaOutputPath ^ "i" in
    let resiContent =
      "let schema: ResGraph.schema<ResGraphContext.context>\n"
    in
    GenerateSchemaUtils.writeIfHasChanges resiOutputPath resiContent;

    if debug && printToStdOut then schemaCode |> print_endline
    else if printToStdOut then
      Printf.printf "{\"status\": \"Success\", \"ok\": true}"
