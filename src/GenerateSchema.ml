open GenerateSchemaTypes
open GenerateSchemaUtils

let variantCasesToEnumValues ~state ~(env : SharedTypes.QueryEnv.t)
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
           addDiagnostic state
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

(* Extracts valid GraphQL types from type exprs *)
let rec findGraphQLType ~env ?(typeContext = Default) ?loc ~state
    ~(full : SharedTypes.full) (typ : Types.type_expr) =
  match typ.desc with
  | Tlink te | Tsubst te | Tpoly (te, []) ->
    findGraphQLType te ?loc ~env ~state ~full ~typeContext
  | Tconstr (Path.Pident {name = "option"}, [unwrappedType], _) -> (
    let inner = findGraphQLType ~env ~state ~full ~typeContext unwrappedType in
    match inner with
    | None -> None
    | Some inner -> Some (Nullable inner))
  | Tconstr (Path.Pident {name = "array"}, [unwrappedType], _) -> (
    let inner =
      findGraphQLType ?loc ~env ~state ~full ~typeContext unwrappedType
    in
    match inner with
    | None -> None
    | Some inner -> Some (List inner))
  | Tconstr (Path.Pident {name = "promise"}, [unwrappedType], _)
    when typeContext = ReturnType ->
    findGraphQLType unwrappedType ?loc ~env ~state ~full ~typeContext
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
          findGraphQLType ?loc ~env ~state ~full ~typeContext typeArg
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
        findGraphQLType te ~loc:type_loc ~env ~state ~full ~typeContext
      | Some (env, {item}) -> (
        let gqlAttribute = extractGqlAttribute ~env ~state item.attributes in
        match (gqlAttribute, item) with
        | Some (ObjectType {interfaces}), {name; kind = Record fields} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          (* TODO: Look up interfaces *)
          noticeObjectType id ~displayName ~state ~env ~interfaces:[]
            ~makeFields:(fun () ->
              fields |> objectTypeFieldsOfRecordFields ~env ~full ~state)
            ~loc:item.decl.type_loc;
          Some (GraphQLObjectType {id; displayName})
        | Some InputObject, {name; kind = Record fields; attributes; decl} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addInputObject id ~state ~makeInputObject:(fun () ->
              {
                id;
                displayName;
                fields =
                  inputObjectFieldsOfRecordFields fields ~env ~full ~state;
                description = attributesToDocstring attributes;
                typeLocation =
                  findTypeLocation item.name ~env ~state ~loc:decl.type_loc
                    ~expectedType:InputObject;
              });
          Some (GraphQLInputObject {id; displayName = capitalizeFirstChar id})
        | ( Some (Interface {interfaces}),
            {name; kind = Record fields; attributes; decl} ) ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addInterface id ~state ~makeInterface:(fun () ->
              {
                id;
                displayName;
                interfaces = [];
                fields = objectTypeFieldsOfRecordFields fields ~env ~full ~state;
                description = attributesToDocstring attributes;
                typeLocation =
                  findTypeLocation item.name ~env ~state ~loc:decl.type_loc
                    ~expectedType:Interface;
              });
          Some (GraphQLInterface {id; displayName = capitalizeFirstChar id})
        | Some Enum, {name; kind = Variant cases} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addEnum id ~state ~makeEnum:(fun () ->
              {
                id;
                displayName;
                values = variantCasesToEnumValues ~state ~env cases;
                description = item.attributes |> attributesToDocstring;
                loc = item.decl.type_loc;
              });
          Some (GraphQLEnum {id; displayName = capitalizeFirstChar id})
        | Some Union, {name; kind = Variant cases} ->
          let id = name in
          let displayName = capitalizeFirstChar id in
          addUnion id ~state ~makeUnion:(fun () ->
              {
                id;
                displayName;
                types = variantCasesToUnionValues cases ~env ~full ~state;
                description = item.attributes |> attributesToDocstring;
                typeLocation =
                  findTypeLocation ~loc:item.decl.type_loc ~state ~env
                    ~expectedType:Union id;
              });
          Some (GraphQLUnion {id; displayName})
        | _ ->
          state
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
        state
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
    state
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

and inputObjectFieldsOfRecordFields ~env ~state ~(full : SharedTypes.full)
    (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         let name = field.fname.txt in
         match
           findGraphQLType field.typ ~loc:field.fname.loc ~full ~env ~state
         with
         | None ->
           state
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

and variantCasesToUnionValues ~env ~state ~full
    (cases : SharedTypes.Constructor.t list) =
  cases
  |> List.filter_map (fun (case : SharedTypes.Constructor.t) ->
         match case.args with
         | Args [(typ, _)] -> (
           match findGraphQLType ~loc:case.cname.loc ~env ~state ~full typ with
           | Some (GraphQLObjectType {id; displayName}) ->
             Some {objectTypeId = id; displayName; loc = case.cname.loc}
           | _ ->
             addDiagnostic state
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
           addDiagnostic state
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

and objectTypeFieldsOfRecordFields ~env ~state ~(full : SharedTypes.full)
    (fields : SharedTypes.field list) =
  fields
  |> List.filter_map (fun (field : SharedTypes.field) ->
         match
           field.attributes |> getFieldAttributeFromRawAttributes ~state ~env
         with
         | None -> None
         | Some attr -> Some (field, attr))
  |> List.filter_map (fun ((field : SharedTypes.field), _attr) ->
         let typ =
           findGraphQLType field.typ ~loc:field.fname.loc ~full ~env ~state
         in
         match typ with
         | None ->
           state
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

let extractResolverFunctionInfo ~env ?loc ~(full : SharedTypes.full)
    ~(state : state) (typ : Types.type_expr) =
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
        ( findGraphQLType typ ?loc ~env ~full ~state,
          findGraphQLType returnType ~typeContext:ReturnType ?loc ~env ~full
            ~state )
      with
      | Some targetGraphQLType, Some returnType ->
        Some (targetGraphQLType, args, returnType)
      | _ -> None)
    | _ -> None)
  | _ -> None

let mapFunctionArgs ~full ~env ~state ~fnLoc
    (args : SharedTypes.typedFnArg list) =
  args
  |> List.filter_map (fun (label, typExpr) ->
         match findGraphQLType ~loc:fnLoc ~full ~env ~state typExpr with
         | None ->
           state
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

  (* Print the interface type holders and getters *)
  state.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
         addWithNewLine
           (Printf.sprintf
              "let i_%s: ref<GraphQLInterfaceType.t> = \
               Obj.magic({\"contents\": Js.null})"
              typ.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => i_%s.contents" typ.displayName
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

  state.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
         addWithNewLine
           (Printf.sprintf "i_%s.contents = GraphQLInterfaceType.make(%s)"
              typ.displayName
              (typ |> GenerateSchemaTypePrinters.printInterfaceType)));

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
  addWithNewLine
    (Printf.sprintf
       "let schema = GraphQLSchemaType.make({\"query\": get_Query()%s%s})"
       (match state.mutation with
       | None -> ""
       | Some _ -> ", \"mutation\": get_Mutation()")
       (match state.subscription with
       | None -> ""
       | Some _ -> ", \"subscription\": get_Subscription()"));
  !code

let rec traverseStructure ?(modulePath = []) ~state ~env ~full
    (structure : SharedTypes.Module.structure) =
  structure.items
  |> List.iter (fun (item : SharedTypes.Module.item) ->
         let gqlAttribute =
           item.attributes |> extractGqlAttribute ~state:!state ~env
         in
         match (item.kind, gqlAttribute) with
         | Module (Structure structure), _ ->
           (* Continue into modules (ignore module aliases etc) *)
           (* Might need to support modules with constraints too. But will
              ignore aliases. *)
           traverseStructure
             ~modulePath:(structure.name :: modulePath)
             ~state ~env ~full structure
         | ( Type ({kind = Record fields; attributes; decl}, _),
             Some (ObjectType {interfaces}) ) ->
           (* @gql.type type someType = {...} *)
           let id = item.name in
           let displayName = capitalizeFirstChar item.name in
           (* TODO: Look up interfaces *)
           noticeObjectType ~env ~loc:decl.type_loc ~state:!state ~interfaces:[]
             ?description:(attributesToDocstring attributes)
             ~displayName
             ~makeFields:(fun () ->
               objectTypeFieldsOfRecordFields fields ~env ~full ~state:!state)
             id
         | Type ({kind = Record fields; attributes; decl}, _), Some InputObject
           ->
           (* @gql.inputObject type someInputObject = {...} *)
           let id = item.name in
           addInputObject id ~state:!state ~makeInputObject:(fun () ->
               {
                 id;
                 displayName = capitalizeFirstChar item.name;
                 fields =
                   inputObjectFieldsOfRecordFields fields ~env ~full
                     ~state:!state;
                 description = attributesToDocstring attributes;
                 typeLocation =
                   findTypeLocation item.name ~env ~state:!state
                     ~loc:decl.type_loc ~expectedType:InputObject;
               })
         | ( Type ({kind = Record fields; attributes; decl}, _),
             Some (Interface {interfaces}) ) ->
           (* @gql.interface type hasName = {...} *)
           let id = item.name in
           addInterface id ~state:!state ~makeInterface:(fun () ->
               {
                 id;
                 displayName = capitalizeFirstChar item.name;
                 fields =
                   objectTypeFieldsOfRecordFields fields ~env ~full
                     ~state:!state;
                 description = attributesToDocstring attributes;
                 interfaces = [];
                 typeLocation =
                   findTypeLocation item.name ~env ~state:!state
                     ~loc:decl.type_loc ~expectedType:Interface;
               })
         | Type (({kind = Variant cases} as item), _), Some Enum ->
           (* @gql.enum type someEnum = Online | Offline | Idle *)
           addEnum item.name ~state:!state ~makeEnum:(fun () ->
               {
                 id = item.name;
                 displayName = capitalizeFirstChar item.name;
                 values = variantCasesToEnumValues ~state:!state ~env cases;
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
                   variantCasesToUnionValues cases ~env ~full ~state:!state;
                 typeLocation =
                   findTypeLocation ~loc:item.decl.type_loc ~env ~state:!state
                     ~expectedType:Union item.name;
               })
             ~state:!state
         | Value typ, Some Field -> (
           (* @gql.field let fullName = (user: user) => {...} *)
           match typ |> TypeUtils.extractType ~env ~package:full.package with
           | Some (Tfunction {typ; env}) -> (
             match
               extractResolverFunctionInfo ~loc:item.loc ~env ~full
                 ~state:!state typ
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
                     mapFunctionArgs ~full ~env ~state:!state ~fnLoc:item.loc
                       args;
                 }
               in
               addFieldToObjectType ~env ~loc:item.loc ~field ~state:!state id
             | _ ->
               !state
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
           | _ ->
             !state
             |> addDiagnostic
                  ~diagnostic:
                    {
                      loc = item.loc;
                      fileUri = env.file.uri;
                      message =
                        Printf.sprintf
                          "This let binding is annotated with @gql.field, but \
                           is not a function. Only functions can represent \
                           GraphQL field resolvers.";
                    })
         | _ -> (
           (* Didn't match. Do some error reporting. *)
           let baseDiagnostic =
             {fileUri = env.file.uri; message = ""; loc = item.loc}
           in
           let add = !state |> addDiagnostic in
           match gqlAttribute with
           | None -> (* Ignore if we had no gql attribute anyway. *) ()
           | Some Field ->
             add
               ~diagnostic:
                 {
                   baseDiagnostic with
                   message =
                     Printf.sprintf
                       "This let binding is annotated with @gql.field, but is \
                        not a function. Only functions can represent GraphQL \
                        field resolvers.";
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
                        not a record. Only records can represent GraphQL input \
                        objects.";
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
                       "This type is annotated with @gql.interface, but is not \
                        a record. Only records can represent GraphQL \
                        interfaces.";
                 }))

let generateSchema ~path ~debug ~outputPath =
  if debug then Printf.printf "generating schema from %s\n\n" path;
  match Cmt.loadFullCmtFromPath ~path with
  | None -> ()
  | Some full ->
    let open SharedTypes in
    let file = full.file in
    let structure = file.structure in
    let env = QueryEnv.fromFile file in
    let state =
      ref
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
        }
    in
    traverseStructure structure ~state ~env ~full;
    if !state.diagnostics |> List.length > 0 then
      !state.diagnostics |> List.rev |> List.map printDiagnostic
      |> String.concat "\n\n" |> print_endline
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
