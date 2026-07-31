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

let isNullableType = function
  | Nullable _ | RescriptNullable _ -> true
  | _ -> false

let rec validateConstValue ~(schemaState : schemaState) typ value =
  let expected () =
    Some
      (Printf.sprintf "Expected a value coercible to `%s`."
         (graphqlTypeToString typ))
  in
  match (typ, value) with
  | (Nullable _ | RescriptNullable _), ConstNull -> None
  | (Nullable inner | RescriptNullable inner), value ->
    validateConstValue ~schemaState inner value
  | _, ConstNull -> expected ()
  | List inner, ConstList values ->
    values |> List.find_map (validateConstValue ~schemaState inner)
  | List inner, value -> validateConstValue ~schemaState inner value
  | Scalar Int, ConstInt _ -> None
  | Scalar Float, (ConstInt _ | ConstFloat _) -> None
  | Scalar String, ConstString _ -> None
  | Scalar Boolean, ConstBoolean _ -> None
  | Scalar ID, (ConstInt _ | ConstString _) -> None
  | GraphQLEnum {id}, ConstEnum value -> (
    match Hashtbl.find_opt schemaState.enums id with
    | Some enum
      when enum.values
           |> List.exists (fun (enumValue : gqlEnumValue) ->
               enumValue.value = value) ->
      None
    | Some enum ->
      Some
        (Printf.sprintf "`%s` is not a value of enum `%s`." value
           enum.displayName)
    | None -> expected ())
  | GraphQLInputObject {id}, ConstObject fields -> (
    match Hashtbl.find_opt schemaState.inputObjects id with
    | None -> expected ()
    | Some inputObject -> (
      let unknownField =
        fields
        |> List.find_opt (fun (name, _) ->
            inputObject.fields
            |> List.exists (fun (field : gqlField) -> field.name = name)
            |> not)
      in
      match unknownField with
      | Some (name, _) ->
        Some
          (Printf.sprintf "Input object `%s` has no field named `%s`."
             inputObject.displayName name)
      | None -> (
        let missingRequiredField =
          inputObject.fields
          |> List.find_opt (fun (field : gqlField) ->
              (not (isNullableType field.typ))
              && Option.is_none field.defaultValue
              && fields |> List.mem_assoc field.name |> not)
        in
        match missingRequiredField with
        | Some field ->
          Some
            (Printf.sprintf "Required input field `%s.%s` is missing."
               inputObject.displayName field.name)
        | None ->
          fields
          |> List.find_map (fun (name, value) ->
              match
                inputObject.fields
                |> List.find_opt (fun (field : gqlField) -> field.name = name)
              with
              | None -> None
              | Some field -> validateConstValue ~schemaState field.typ value)))
    )
  | GraphQLInputUnion {id}, ConstObject fields -> (
    match Hashtbl.find_opt schemaState.inputUnions id with
    | None -> expected ()
    | Some inputUnion -> (
      let nonNullFields =
        fields |> List.filter (fun (_, value) -> value <> ConstNull)
      in
      if List.length nonNullFields <> 1 then
        Some
          (Printf.sprintf
             "OneOf input `%s` requires exactly one non-null field."
             inputUnion.displayName)
      else
        let unknownField =
          fields
          |> List.find_opt (fun (name, _) ->
              inputUnion.members
              |> List.exists (fun (member : gqlInputUnionMember) ->
                  member.fieldName = name)
              |> not)
        in
        match unknownField with
        | Some (name, _) ->
          Some
            (Printf.sprintf "OneOf input `%s` has no field named `%s`."
               inputUnion.displayName name)
        | None ->
          nonNullFields
          |> List.find_map (fun (name, value) ->
              match
                inputUnion.members
                |> List.find_opt (fun (member : gqlInputUnionMember) ->
                    member.fieldName = name)
              with
              | None -> None
              | Some member -> validateConstValue ~schemaState member.typ value)
      ))
  | ( GraphQLScalar _,
      (ConstInt _ | ConstFloat _ | ConstString _ | ConstBoolean _ | ConstEnum _)
    ) ->
    None
  | EmptyPayload, ConstBoolean _ -> None
  | ( ( InjectContext | InjectInfo | InjectInterfaceTypename _
      | GraphQLObjectType _ | GraphQLUnion _ | GraphQLInterface _ ),
      _ ) ->
    expected ()
  | _ -> expected ()

let validateInputFields ~schemaState ~(parentTypeName : string)
    (fields : gqlField list) =
  validateFields ~schemaState ~parentTypeName fields;
  fields
  |> List.iter (fun (field : gqlField) ->
      (match field.defaultValue with
      | None -> ()
      | Some value -> (
        match validateConstValue ~schemaState field.typ value with
        | None -> ()
        | Some message ->
          schemaState
          |> addDiagnostic
               ~diagnostic:
                 {
                   loc = field.loc;
                   fileUri = field.fileUri;
                   message =
                     Printf.sprintf
                       "Invalid default for input field `%s.%s`: %s"
                       parentTypeName field.name message;
                 }));
      if
        Option.is_some field.deprecationReason
        && (not (isNullableType field.typ))
        && Option.is_none field.defaultValue
      then
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {
                 loc = field.loc;
                 fileUri = field.fileUri;
                 message =
                   Printf.sprintf
                     "Required input field `%s.%s` cannot be deprecated \
                      without a default value."
                     parentTypeName field.name;
               })

let validateDirectiveDefinitions (schemaState : schemaState) =
  let reservedNames =
    ["skip"; "include"; "deprecated"; "specifiedBy"; "oneOf"]
  in
  schemaState.directiveDefinitions
  |> Hashtbl.iter (fun _name (definition : gqlDirectiveDefinition) ->
      validateName ~name:definition.name
        ~typeLocation:(Concrete definition.typeLocation) schemaState;
      if List.mem definition.name reservedNames then
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {
                 loc = definition.typeLocation.loc;
                 fileUri = definition.typeLocation.fileUri;
                 message =
                   Printf.sprintf
                     "`@%s` is a GraphQL-specified directive and cannot be \
                      redefined."
                     definition.name;
               };
      let seenLocations = Hashtbl.create (List.length definition.locations) in
      definition.locations
      |> List.iter (fun location ->
          if Hashtbl.mem seenLocations location then
            schemaState
            |> addDiagnostic
                 ~diagnostic:
                   {
                     loc = definition.typeLocation.loc;
                     fileUri = definition.typeLocation.fileUri;
                     message =
                       Printf.sprintf
                         "Directive `@%s` declares location `%s` more than \
                          once."
                         definition.name
                         (GenerateSchemaDirectiveUtils.locationToString location);
                   }
          else Hashtbl.add seenLocations location ());
      definition.arguments
      |> List.iter (fun (argument : gqlDirectiveArgument) ->
          validateName ~name:argument.name
            ~typeLocation:
              (mkTypeLocation ~typeName:argument.name
                 ~fileName:definition.typeLocation.fileName
                 ~fileUri:definition.typeLocation.fileUri ~loc:argument.loc)
            schemaState;
          (match argument.defaultValue with
          | None -> ()
          | Some value -> (
            match validateConstValue ~schemaState argument.typ value with
            | None -> ()
            | Some message ->
              schemaState
              |> addDiagnostic
                   ~diagnostic:
                     {
                       loc = argument.loc;
                       fileUri = definition.typeLocation.fileUri;
                       message =
                         Printf.sprintf
                           "Invalid default for directive argument `@%s(%s:)`: \
                            %s"
                           definition.name argument.name message;
                     }));
          if
            Option.is_some argument.deprecationReason
            && (not (isNullableType argument.typ))
            && Option.is_none argument.defaultValue
          then
            schemaState
            |> addDiagnostic
                 ~diagnostic:
                   {
                     loc = argument.loc;
                     fileUri = definition.typeLocation.fileUri;
                     message =
                       Printf.sprintf
                         "Required directive argument `@%s(%s:)` cannot be \
                          deprecated without a default value."
                         definition.name argument.name;
                   }))

let validateDirectiveApplications (schemaState : schemaState) =
  schemaState.appliedDirectives
  |> Hashtbl.iter (fun target applications ->
      let counts = Hashtbl.create (List.length applications) in
      applications
      |> List.iter (fun (application : gqlDirectiveApplication) ->
          match
            Hashtbl.find_opt schemaState.directiveDefinitions application.name
          with
          | None ->
            schemaState
            |> addDiagnostic
                 ~diagnostic:
                   {
                     loc = application.loc;
                     fileUri = application.fileUri;
                     message =
                       Printf.sprintf
                         "Directive `@%s` is not defined in this schema."
                         application.name;
                   }
          | Some definition ->
            let location =
              GenerateSchemaDirectiveUtils.locationForTarget target
            in
            if not (List.mem location definition.locations) then
              schemaState
              |> addDiagnostic
                   ~diagnostic:
                     {
                       loc = application.loc;
                       fileUri = application.fileUri;
                       message =
                         Printf.sprintf
                           "Directive `@%s` cannot be used on `%s`; its \
                            definition does not include `%s`."
                           application.name
                           (GenerateSchemaDirectiveUtils.targetToString target)
                           (GenerateSchemaDirectiveUtils.locationToString
                              location);
                     };
            let previousCount =
              Hashtbl.find_opt counts application.name
              |> Option.value ~default:0
            in
            Hashtbl.replace counts application.name (previousCount + 1);
            if previousCount > 0 && not definition.repeatable then
              schemaState
              |> addDiagnostic
                   ~diagnostic:
                     {
                       loc = application.loc;
                       fileUri = application.fileUri;
                       message =
                         Printf.sprintf
                           "Directive `@%s` is not repeatable on `%s`."
                           application.name
                           (GenerateSchemaDirectiveUtils.targetToString target);
                     };
            application.arguments
            |> List.iter (fun (name, value) ->
                match
                  definition.arguments
                  |> List.find_opt (fun (argument : gqlDirectiveArgument) ->
                      argument.name = name)
                with
                | None ->
                  schemaState
                  |> addDiagnostic
                       ~diagnostic:
                         {
                           loc = application.loc;
                           fileUri = application.fileUri;
                           message =
                             Printf.sprintf
                               "Directive `@%s` has no argument named `%s`."
                               application.name name;
                         }
                | Some argument -> (
                  match validateConstValue ~schemaState argument.typ value with
                  | None -> ()
                  | Some message ->
                    schemaState
                    |> addDiagnostic
                         ~diagnostic:
                           {
                             loc = application.loc;
                             fileUri = application.fileUri;
                             message =
                               Printf.sprintf "Invalid value for `@%s(%s:)`: %s"
                                 application.name name message;
                           }));
            definition.arguments
            |> List.iter (fun (argument : gqlDirectiveArgument) ->
                if
                  (not (isNullableType argument.typ))
                  && Option.is_none argument.defaultValue
                  && application.arguments
                     |> List.mem_assoc argument.name
                     |> not
                then
                  schemaState
                  |> addDiagnostic
                       ~diagnostic:
                         {
                           loc = application.loc;
                           fileUri = application.fileUri;
                           message =
                             Printf.sprintf
                               "Directive `@%s` requires argument `%s`."
                               application.name argument.name;
                         })))

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

let validateSchema (schemaState : schemaState) =
  validateRootTypes schemaState;
  validateInterfaceImplementationCycles schemaState;
  validateDirectiveDefinitions schemaState;
  validateDirectiveApplications schemaState;

  schemaState.scalars
  |> Hashtbl.iter (fun _name (typ : gqlScalar) ->
      validateName ~name:typ.displayName
        ~typeLocation:(Concrete typ.typeLocation) schemaState);

  schemaState.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
      validateFields ~schemaState ~parentTypeName:typ.displayName typ.fields);

  schemaState.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
      validateInputFields ~schemaState ~parentTypeName:typ.displayName
        typ.fields);

  schemaState.enums
  |> Hashtbl.iter (fun _name (typ : gqlEnum) ->
      (* No need to validate each case, ReScript has already done it for us. *)
      validateName ~name:typ.displayName ~typeLocation:typ.typeLocation
        schemaState);

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
