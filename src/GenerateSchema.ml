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
                     "The variant case `%s` of the GraphQL enum variant `%s` \
                      has a payload. Variants tagged as @gql.enum can only \
                      have cases without payloads. "
                     case.cname.txt (case.typeDecl |> fst);
                 fileUri = env.file.uri;
               };
           None)

type typeContext = Default | ReturnType

(* Extracts valid GraphQL types from type exprs *)
let rec findGraphQLType ~env ?(typeContext = Default) ~debug ?loc ~schemaState
    ~(full : SharedTypes.full) (typ : Types.type_expr) =
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
        | Some ObjectType, {attributes; name; kind = Record fields} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          noticeObjectType id ~displayName ~debug ~schemaState ~env
            ?description:(GenerateSchemaUtils.attributesToDocstring attributes)
            ~makeFields:(fun () ->
              fields
              |> objectTypeFieldsOfRecordFields ~env ~debug ~full ~schemaState)
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
                typeLocation =
                  findTypeLocation item.name ~env ~schemaState
                    ~loc:decl.type_loc ~expectedType:InputObject;
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
                loc = item.decl.type_loc;
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
                  variantCasesToUnionValues cases ~env ~full ~schemaState ~debug;
                description = item.attributes |> attributesToDocstring;
                typeLocation =
                  findTypeLocation ~loc:item.decl.type_loc ~schemaState ~env
                    ~expectedType:Union id;
              });
          Some (GraphQLUnion {id; displayName})
        | Some (InterfaceResolver {interfaceId}), {kind = Variant _} ->
          Some
            (GraphQLInterface
               {id = interfaceId; displayName = capitalizeFirstChar interfaceId})
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
               args = [];
               description = field.attributes |> attributesToDocstring;
               deprecationReason = field.deprecated;
               loc = field.fname.loc;
             })

and variantCasesToUnionValues ~env ~debug ~schemaState ~full
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | Args [(typ, _)] -> (
           match
             findGraphQLType ~debug ~loc:case.cname.loc ~env ~schemaState ~full
               typ
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
                       "The payload of the variant case `%s` of the GraphQL \
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
                     "The payload of the variant case `%s` of the GraphQL \
                      union variant `%s` is not a single payload. The payload \
                      needs to be a single type representing a GraphQL object, \
                      meaning it's annotated with @gql.type."
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
         let typ =
           findGraphQLType field.typ ~debug ~loc:field.fname.loc ~full ~env
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
    ~(schemaState : schemaState) ~debug (typ : Types.type_expr) =
  match typ |> TypeUtils.extractType ~env ~package:full.package with
  | Some (Tfunction {typ; env}) -> (
    let args, returnType =
      TypeUtils.extractFunctionType ~env ~package:full.package typ
    in
    (* The first arg should point to the type this resolver is for. *)
    match List.nth_opt args 0 with
    | Some (Nolabel, typ) -> (
      if debug then Printf.printf "Checking for type to attach resolver to\n";
      match
        ( findGraphQLType typ ~debug ?loc ~env ~full ~schemaState,
          findGraphQLType returnType ~debug ~typeContext:ReturnType ?loc ~env
            ~full ~schemaState )
      with
      | Some targetGraphQLType, Some returnType ->
        Some (targetGraphQLType, args, returnType)
      | _ -> None)
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

and traverseStructure ?(modulePath = []) ?originModule
    ~(schemaState : schemaState) ~env ~debug ~full
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
           | Type ({kind = Record fields; attributes; decl}, _), Some ObjectType
             ->
             (* @gql.type type someType = {...} *)
             let id = item.name in
             let displayName = capitalizeFirstChar item.name in
             noticeObjectType ~env ~debug ~loc:decl.type_loc ~schemaState
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
                   typeLocation =
                     findTypeLocation item.name ~env ~schemaState
                       ~loc:decl.type_loc ~expectedType:InputObject;
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
             addEnum item.name ~schemaState ~debug ~makeEnum:(fun () ->
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
                     variantCasesToUnionValues cases ~env ~full ~schemaState
                       ~debug;
                   typeLocation =
                     findTypeLocation ~loc:item.decl.type_loc ~env ~schemaState
                       ~expectedType:Union item.name;
                 })
               ~schemaState ~debug
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
               schemaState
               |> addDiagnostic
                    ~diagnostic:
                      {
                        loc = item.decl.type_loc;
                        fileUri = env.file.uri;
                        message =
                          Printf.sprintf
                            "This GraphQL scalar maps to something that \
                             requires custom parsing and serializing, and \
                             that's currently not supported in ResGraph (but \
                             coming soon). Please use only primitives that \
                             produces valid JSON for now.";
                      })
           | Value typ, Some Field -> (
             (* @gql.field let fullName = (user: user) => {...} *)
             match typ |> TypeUtils.extractType ~env ~package:full.package with
             | Some (Tfunction {typ; env}) -> (
               match
                 extractResolverFunctionInfo ~loc:item.loc ~env ~full
                   ~schemaState ~debug typ
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
                       mapFunctionArgs ~full ~debug ~env ~schemaState
                         ~fnLoc:item.loc args;
                   }
                 in
                 addFieldToObjectType ~env ~loc:item.loc ~field ~schemaState id
               | Some (GraphQLInterface {id}, args, returnType) ->
                 (* Resolver for interface type. *)
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
                       mapFunctionArgs ~full ~debug ~env ~schemaState
                         ~fnLoc:item.loc args;
                   }
                 in
                 addFieldToInterfaceType ~env ~loc:item.loc ~field ~schemaState
                   id
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
           | _, None -> ( (* Ignore values with no gql attribute. *) )
           | v, Some _ -> (
             Printf.printf "fail: %s\n"
               (match v with
               | Value _ -> "value"
               | Type ({kind = Abstract (Some (_p, _t))}, _) ->
                 "Type"
                 ^ (_t |> List.map Shared.typeToString |> String.concat ", ")
               | Type _ -> "Type2"
               | Module _ -> "module");
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

let generateSchema ~writeStateFile ~sourceFolder ~debug ~outputFolder =
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
            loc =
              {
                Location.loc_start = Lexing.dummy_pos;
                loc_end = Lexing.dummy_pos;
                loc_ghost = true;
              };
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
         | "ResGraph" | "ResGraphSchema" | "ResGraphSchemaAssets"
         | "GraphQLYoga" | "Errors" | "ResGraph__GraphQLJs" ->
           ()
         | _ -> (
           match Hashtbl.find package.pathsForModule file with
           | IntfAndImpl {res} | Impl {res} -> processFileAtPath res |> ignore
           | _ -> ()));

  let processedSchema = processSchema schemaState in

  if schemaState.diagnostics |> List.length > 0 then
    Printf.printf
      "{\n  \"status\": \"Error\",\n  \"errors\": \n    [\n      %s\n    ]\n}"
      (schemaState.diagnostics |> List.rev |> List.map printDiagnostic
     |> String.concat ",\n")
  else
    let schemaCode =
      GenerateSchemaTypePrinters.printSchemaJsFile schemaState processedSchema
      |> formatCode
    in
    let assetCode =
      GenerateSchemaTypePrinters.printSchemaAssets ~schemaState ~processedSchema
      |> formatCode
    in

    if writeStateFile then
      GenerateSchemaUtils.writeStateFile ~package ~schemaState ~processedSchema;

    let schemaOutputPath = outputFolder ^ "/ResGraphSchema.res" in
    let assetsOutputPath = outputFolder ^ "/ResGraphSchemaAssets.res" in

    (* TODO: Do this in parallell in some fancy way *)

    (* Write generated schema *)
    (* Write implementation file *)
    GenerateSchemaUtils.writeIfHasChanges schemaOutputPath schemaCode;

    (* Write resi file *)
    let resiOutputPath = schemaOutputPath ^ "i" in
    let resiContent =
      "let schema: ResGraph.schema<ResGraphContext.context>\n"
    in
    GenerateSchemaUtils.writeIfHasChanges resiOutputPath resiContent;

    (* Write assets file *)
    GenerateSchemaUtils.writeIfHasChanges assetsOutputPath assetCode;

    if debug then schemaCode |> print_endline
    else Printf.printf "{\"status\": \"Success\", \"ok\": true}"
