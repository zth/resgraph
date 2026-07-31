open GenerateSchemaTypes
open GenerateSchemaDiagnostics

(*
  This aims to implement the most important validations from graphql-js directly
  in ResGraph. We're fine with letting some errors through, having graphql-js
  report them at runtime instead. But the large bulk of errors you'd normally
  encounter should be reimplemented here, so the DX of ResGraph is good enough.
*)

let emptyLoc =
  {
    Location.loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos;
    loc_ghost = true;
  }

let mkTypeLocation ~typeName ~fileName ~fileUri ~loc =
  Concrete {fileName; fileUri; modulePath = []; typeName; loc}

let validateName ~name ~(typeLocation : typeLocation)
    (schemaState : schemaState) =
  match typeLocation with
  | Synthetic _ -> ()
  | Concrete typeLocation ->
    if Utils.startsWith name "__" then
      schemaState
      |> addDiagnostic
           ~diagnostic:
             {
               loc = typeLocation.loc;
               fileUri = typeLocation.fileUri;
               message =
                 Printf.sprintf
                   "Name \"%s\" must not begin with \"__\", which is reserved \
                    by GraphQL introspection."
                   name;
             }

let validateFieldNameUniqueness ~schemaState ~(parentTypeName : string)
    (fields : gqlField list) =
  let seen = Hashtbl.create (List.length fields) in
  fields
  |> List.iter (fun (field : gqlField) ->
      match Hashtbl.find_opt seen field.name with
      | None -> Hashtbl.add seen field.name field
      | Some firstField ->
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {
                 loc = field.loc;
                 fileUri = field.fileUri;
                 message =
                   Printf.sprintf
                     "Field `%s` appears more than once on GraphQL type `%s`. \
                      Rename one of the fields or change its @as attribute. \
                      The first field was declared in %s."
                     field.name parentTypeName firstField.fileName;
               })

let validateFields ~schemaState ~(parentTypeName : string)
    (fields : gqlField list) =
  validateFieldNameUniqueness ~schemaState ~parentTypeName fields;
  fields
  |> List.iter (fun (f : gqlField) ->
      validateName ~name:f.name
        ~typeLocation:
          (mkTypeLocation ~typeName:f.name ~loc:f.loc ~fileName:f.fileName
             ~fileUri:f.fileUri)
        schemaState)

let scalarToString (scalar : scalar) =
  match scalar with
  | Int -> "Int"
  | Float -> "Float"
  | String -> "String"
  | Boolean -> "Boolean"
  | ID -> "ID"

let rec graphqlTypeToString ?(nullable = false) (typ : graphqlType) =
  let nullableSuffix = if nullable then "" else "!" in
  match typ with
  | List inner ->
    Printf.sprintf "[%s]%s" (graphqlTypeToString inner) nullableSuffix
  | Nullable inner | RescriptNullable inner ->
    graphqlTypeToString ~nullable:true inner
  | Scalar scalar -> scalarToString scalar ^ nullableSuffix
  | EmptyPayload -> graphqlTypeToString ~nullable:true (Scalar Boolean)
  | InjectContext -> "<context>"
  | InjectInfo -> "<info>"
  | InjectInterfaceTypename {interfaceId = intfId} -> intfId ^ nullableSuffix
  | GraphQLObjectType {displayName}
  | GraphQLInputObject {displayName}
  | GraphQLInputUnion {displayName}
  | GraphQLEnum {displayName}
  | GraphQLUnion {displayName}
  | GraphQLInterface {displayName}
  | GraphQLScalar {displayName} ->
    displayName ^ nullableSuffix

let nullableInner (typ : graphqlType) =
  match typ with
  | Nullable inner | RescriptNullable inner -> Some inner
  | EmptyPayload -> Some (Scalar Boolean)
  | _ -> None

let rec sameGraphQLType left right =
  match (nullableInner left, nullableInner right) with
  | Some left, Some right -> sameGraphQLType left right
  | Some _, None | None, Some _ -> false
  | None, None -> (
    match (left, right) with
    | List left, List right -> sameGraphQLType left right
    | Scalar left, Scalar right -> left = right
    | InjectContext, InjectContext -> true
    | InjectInfo, InjectInfo -> true
    | ( InjectInterfaceTypename {interfaceId = left},
        InjectInterfaceTypename {interfaceId = right} ) ->
      left = right
    | GraphQLObjectType left, GraphQLObjectType right -> left.id = right.id
    | GraphQLInputObject left, GraphQLInputObject right -> left.id = right.id
    | GraphQLEnum left, GraphQLEnum right -> left.id = right.id
    | GraphQLUnion left, GraphQLUnion right -> left.id = right.id
    | GraphQLInterface left, GraphQLInterface right -> left.id = right.id
    | GraphQLScalar left, GraphQLScalar right -> left.id = right.id
    | GraphQLInputUnion left, GraphQLInputUnion right ->
      left.id = right.id
      && left.inlineRecords = right.inlineRecords
      && left.emptyPayloads = right.emptyPayloads
    | EmptyPayload, EmptyPayload -> true
    | _ -> false)

let interfaceTransitivelyImplements schemaState interfaceId expectedId =
  let rec loop ~visited interfaceId =
    if List.mem interfaceId visited then false
    else
      interfaceId = expectedId
      ||
      match Hashtbl.find_opt schemaState.interfaces interfaceId with
      | None -> false
      | Some intf ->
        intf.interfaces
        |> List.exists (fun nextInterfaceId ->
            loop ~visited:(interfaceId :: visited) nextInterfaceId)
  in
  loop ~visited:[] interfaceId

let namedOutputSubtype schemaState ~actual ~expected =
  sameGraphQLType actual expected
  ||
  match (actual, expected) with
  | GraphQLObjectType actualObject, GraphQLInterface expectedInterface -> (
    match Hashtbl.find_opt schemaState.types actualObject.id with
    | None -> false
    | Some typ ->
      typ.interfaces
      |> List.exists (fun intfId ->
          interfaceTransitivelyImplements schemaState intfId
            expectedInterface.id))
  | GraphQLInterface actualInterface, GraphQLInterface expectedInterface ->
    interfaceTransitivelyImplements schemaState actualInterface.id
      expectedInterface.id
  | GraphQLObjectType actualObject, GraphQLUnion expectedUnion -> (
    match Hashtbl.find_opt schemaState.unions expectedUnion.id with
    | None -> false
    | Some union ->
      union.types
      |> List.exists (fun (member : gqlUnionMember) ->
          member.objectTypeId = actualObject.id))
  | _ -> false

let rec isOutputSubtype schemaState ~actual ~expected =
  match nullableInner expected with
  | Some expectedInner -> (
    match nullableInner actual with
    | Some actualInner ->
      isOutputSubtype schemaState ~actual:actualInner ~expected:expectedInner
    | None -> isOutputSubtype schemaState ~actual ~expected:expectedInner)
  | None -> (
    match nullableInner actual with
    | Some _ -> false
    | None -> (
      match (actual, expected) with
      | List actual, List expected ->
        isOutputSubtype schemaState ~actual ~expected
      | _ -> namedOutputSubtype schemaState ~actual ~expected))

let isRequiredInputType typ = nullableInner typ |> Option.is_none

let findFieldByName fields name =
  fields |> List.find_opt (fun (field : gqlField) -> field.name = name)

let findArgByName args name =
  args |> List.find_opt (fun (arg : gqlArg) -> arg.name = name)

let isSchemaVisibleArg (arg : gqlArg) =
  match arg.typ with
  | InjectContext | InjectInfo | InjectInterfaceTypename _ -> false
  | _ -> true

let addInterfaceImplementationDiagnostic schemaState ~loc ~fileUri message =
  schemaState |> addDiagnostic ~diagnostic:{loc; fileUri; message}

let validateInterfaceFieldArguments ~schemaState ~loc ~fileUri
    ~implementingTypeName ~interfaceName ~(implementationField : gqlField)
    ~(interfaceField : gqlField) =
  let interfaceArgs = interfaceField.args |> List.filter isSchemaVisibleArg in
  let implementationArgs =
    implementationField.args |> List.filter isSchemaVisibleArg
  in
  interfaceArgs
  |> List.iter (fun (interfaceArg : gqlArg) ->
      match findArgByName implementationArgs interfaceArg.name with
      | None ->
        addInterfaceImplementationDiagnostic schemaState ~loc ~fileUri
          (Printf.sprintf
             "`%s` cannot implement `%s`: field `%s` is missing argument `%s`."
             implementingTypeName interfaceName interfaceField.name
             interfaceArg.name)
      | Some implementationArg ->
        if sameGraphQLType implementationArg.typ interfaceArg.typ = false then
          addInterfaceImplementationDiagnostic schemaState ~loc ~fileUri
            (Printf.sprintf
               "`%s` cannot implement `%s`: argument `%s` on field `%s` has \
                type `%s` but the interface requires `%s`."
               implementingTypeName interfaceName interfaceArg.name
               interfaceField.name
               (graphqlTypeToString implementationArg.typ)
               (graphqlTypeToString interfaceArg.typ)));
  implementationArgs
  |> List.iter (fun (implementationArg : gqlArg) ->
      match findArgByName interfaceArgs implementationArg.name with
      | Some _ -> ()
      | None ->
        if isRequiredInputType implementationArg.typ then
          addInterfaceImplementationDiagnostic schemaState ~loc ~fileUri
            (Printf.sprintf
               "`%s` cannot implement `%s`: field `%s` adds required argument \
                `%s`, which is not allowed by GraphQL interface \
                implementations."
               implementingTypeName interfaceName interfaceField.name
               implementationArg.name))

let validateInterfaceImplementation ~schemaState ~loc ~fileUri
    ~implementingTypeName ~(implementingFields : gqlField list)
    ~(interface : gqlInterface) =
  interface.fields
  |> List.iter (fun (interfaceField : gqlField) ->
      match findFieldByName implementingFields interfaceField.name with
      | None ->
        addInterfaceImplementationDiagnostic schemaState ~loc ~fileUri
          (Printf.sprintf
             "`%s` declares @gql.implements(\"%s\") but is missing field `%s: \
              %s`."
             implementingTypeName interface.displayName interfaceField.name
             (graphqlTypeToString interfaceField.typ))
      | Some implementationField ->
        if
          isOutputSubtype schemaState ~actual:implementationField.typ
            ~expected:interfaceField.typ
          = false
        then
          addInterfaceImplementationDiagnostic schemaState ~loc ~fileUri
            (Printf.sprintf
               "`%s` cannot implement `%s`: field `%s` has type `%s` but the \
                interface requires `%s`."
               implementingTypeName interface.displayName interfaceField.name
               (graphqlTypeToString implementationField.typ)
               (graphqlTypeToString interfaceField.typ));
        validateInterfaceFieldArguments ~schemaState ~loc ~fileUri
          ~implementingTypeName ~interfaceName:interface.displayName
          ~implementationField ~interfaceField)

let validateRootTypes (schemaState : schemaState) =
  match schemaState.query with
  | None ->
    schemaState
    |> addDiagnostic
         ~diagnostic:
           {
             loc = emptyLoc;
             fileUri = schemaState.rootFileUri;
             message = "You must define at least a `query` type in your schema.";
           }
  | Some _ -> ()

let validateInterfaceImplementationCycles (schemaState : schemaState) =
  let rec hasCycle ~targetInterfaceId ~visited intfId =
    if List.mem intfId visited then false
    else
      match Hashtbl.find_opt schemaState.interfaces intfId with
      | None -> false
      | Some intf ->
        intf.interfaces
        |> List.exists (fun parentInterfaceId ->
            parentInterfaceId = targetInterfaceId
            || hasCycle ~targetInterfaceId ~visited:(intfId :: visited)
                 parentInterfaceId)
  in
  schemaState.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
      if hasCycle ~targetInterfaceId:typ.id ~visited:[] typ.id then
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {
                 loc = typ.typeLocation.loc;
                 fileUri = typ.typeLocation.fileUri;
                 message =
                   Printf.sprintf
                     "Interface `%s` cannot implement itself, directly or \
                      through another interface."
                     typ.displayName;
               })

type graphqlTypeNameEntry = {
  name: string;
  kind: string;
  loc: Location.t;
  fileUri: Uri.t;
}

let validateTypeNameUniqueness (schemaState : schemaState) =
  let entries = ref [] in
  let add ~name ~kind ~loc ~fileUri =
    entries := {name; kind; loc; fileUri} :: !entries
  in
  let addTypeLocation ~name ~kind = function
    | Concrete typeLocation ->
      add ~name ~kind ~loc:typeLocation.loc ~fileUri:typeLocation.fileUri
    | Synthetic {fileUri} -> add ~name ~kind ~loc:emptyLoc ~fileUri
  in
  schemaState.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
      match typ.typeLocation with
      | None -> ()
      | Some location ->
        addTypeLocation ~name:typ.displayName ~kind:"object type" location);
  schemaState.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
      match typ.typeLocation with
      | None -> ()
      | Some location ->
        add ~name:typ.displayName ~kind:"input object" ~loc:location.loc
          ~fileUri:location.fileUri);
  schemaState.inputUnions
  |> Hashtbl.iter (fun _name (typ : gqlInputUnionType) ->
      add ~name:typ.displayName ~kind:"input union" ~loc:typ.typeLocation.loc
        ~fileUri:typ.typeLocation.fileUri);
  schemaState.enums
  |> Hashtbl.iter (fun _name (typ : gqlEnum) ->
      addTypeLocation ~name:typ.displayName ~kind:"enum" typ.typeLocation);
  schemaState.unions
  |> Hashtbl.iter (fun _name (typ : gqlUnion) ->
      addTypeLocation ~name:typ.displayName ~kind:"union" typ.typeLocation);
  schemaState.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
      add ~name:typ.displayName ~kind:"interface" ~loc:typ.typeLocation.loc
        ~fileUri:typ.typeLocation.fileUri);
  schemaState.scalars
  |> Hashtbl.iter (fun _name (typ : gqlScalar) ->
      add ~name:typ.displayName ~kind:"scalar" ~loc:typ.typeLocation.loc
        ~fileUri:typ.typeLocation.fileUri);
  let registered = Hashtbl.create (List.length !entries) in
  !entries
  |> List.sort (fun left right ->
      compare
        (left.name, left.kind, Uri.toPath left.fileUri, Loc.toString left.loc)
        ( right.name,
          right.kind,
          Uri.toPath right.fileUri,
          Loc.toString right.loc ))
  |> List.iter (fun entry ->
      match Hashtbl.find_opt registered entry.name with
      | None -> Hashtbl.add registered entry.name entry
      | Some previous ->
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {
                 loc = entry.loc;
                 fileUri = entry.fileUri;
                 message =
                   Printf.sprintf
                     "GraphQL type name `%s` is used by both a %s and a %s. \
                      Type names must be unique across all GraphQL kinds."
                     entry.name previous.kind entry.kind;
               })

let validateSchema (schemaState : schemaState) =
  validateRootTypes schemaState;
  validateInterfaceImplementationCycles schemaState;

  schemaState.scalars
  |> Hashtbl.iter (fun _name (typ : gqlScalar) ->
      validateName ~name:typ.displayName
        ~typeLocation:(Concrete typ.typeLocation) schemaState);

  schemaState.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
      validateFields ~schemaState ~parentTypeName:typ.displayName typ.fields);

  schemaState.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
      validateFields ~schemaState ~parentTypeName:typ.displayName typ.fields);

  schemaState.enums
  |> Hashtbl.iter (fun _name (typ : gqlEnum) ->
      (* No need to validate each case, ReScript has already done it for us. *)
      validateName ~name:typ.displayName ~typeLocation:typ.typeLocation
        schemaState);
  validateTypeNameUniqueness schemaState;

  schemaState.unions
  |> Hashtbl.iter (fun _name (typ : gqlUnion) ->
      (* No need to validate each case, ReScript has already done it for us. *)
      validateName ~name:typ.displayName ~typeLocation:typ.typeLocation
        schemaState);

  schemaState.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
      (* Subtype rules etc for interface fields are a bit complicated, so we
            let graphql-js do it at runtime instead. *)
      validateFields ~schemaState ~parentTypeName:typ.displayName typ.fields)
