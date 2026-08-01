open GenerateSchemaTypes
open GenerateSchemaDiagnostics

let findInterfacesOfType code ~schemaState =
  let {Res_driver.parsetree = structure} =
    Res_driver.parse_implementation_from_source ~for_printer:true ~source:code
      ~display_filename:"-"
  in
  match structure with
  | [{pstr_desc = Pstr_type (_, [{ptype_kind = Ptype_record fields}])}] -> (
    match
      fields
      |> List.filter_map (fun (field : Parsetree.label_declaration) ->
          match field with
          | {
           pld_name = {txt = "..."};
           pld_type = {ptyp_desc = Ptyp_constr (loc, _)};
          } ->
            let interfaceName = loc.txt |> Longident.last in
            if Hashtbl.mem schemaState.interfaces interfaceName then
              Some interfaceName
            else None
          | _ -> None)
    with
    | [] -> None
    | v -> Some v)
  | _ -> None

let validAttributes =
  [
    ("gql.type", "Indicates that the annotated record is a GraphQL Object Type.");
    ( "gql.interface",
      "Indicates that the annotated record is a GraphQL interface." );
    ( "gql.implements",
      "Indicates that the annotated object type or interface implements a \
       GraphQL interface." );
    ("gql.interfaceResolver", "");
    ("gql.field", "");
    ("gql.query", "Defines a field on the query root without a source argument.");
    ( "gql.mutation",
      "Defines a field on the mutation root without a source argument." );
    ( "gql.subscription",
      "Defines a field on the subscription root without a source argument." );
    ("gql.enum", "");
    ("gql.union", "");
    ("gql.inputObject", "");
    ("gql.inputUnion", "");
    ("gql.scalar", "");
    ("gql.directive", "Defines a typed GraphQL directive.");
    ("gql.schema", "Defines GraphQL schema metadata and root mappings.");
    ("gql.annotate", "Applies a GraphQL directive to a schema element.");
    ("gql.description", "Describes a GraphQL resolver argument.");
    ("gql.default", "Defines a GraphQL constant default value.");
    ("gql.authorize", "Attaches a typed authorization function.");
    ("gql.public", "Marks a field public with a required reason.");
  ]

let hasGqlAnnotation attributes =
  attributes
  |> List.exists (fun ((name, _payload) : Parsetree.attribute) ->
      Utils.startsWith name.txt "gql.")

let extractGqlAttribute ~(schemaState : GenerateSchemaTypes.schemaState)
    ~(env : SharedTypes.QueryEnv.t) (attributes : Parsetree.attributes) =
  attributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
      match String.split_on_char '.' name.txt with
      | ["gql"; "type"] -> Some ObjectType
      | ["gql"; "interface"] -> Some Interface
      | ["gql"; "implements"] -> None
      | ["gql"; "scalar"] -> Some Scalar
      | ["gql"; "interfaceResolver"] -> (
        match payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc =
                          Pexp_constant (Pconst_string (interfaceId, _));
                      },
                      _ );
              };
            ] ->
          Some (InterfaceResolver {interfaceId})
        | _ ->
          schemaState
          |> addDiagnostic
               ~diagnostic:
                 {
                   loc = name.loc;
                   fileUri = env.file.uri;
                   message =
                     Printf.sprintf
                       "`%s` is annotated as @gql.interfaceResolver but did \
                        not have a string literal as payload."
                       name.txt;
                 };
          None)
      | ["gql"; "field"] -> Some Field
      | ["gql"; "query"] -> Some QueryField
      | ["gql"; "mutation"] -> Some MutationField
      | ["gql"; "subscription"] -> Some SubscriptionField
      | ["gql"; "enum"] -> Some Enum
      | ["gql"; "union"] -> Some Union
      | ["gql"; "inputObject"] -> Some InputObject
      | ["gql"; "inputUnion"] -> Some InputUnion
      | ["gql"; "directive"] -> Some Directive
      | ["gql"; "schema"] -> Some Schema
      | ["gql"; "annotate"]
      | ["gql"; "default"]
      | ["gql"; "authorize"]
      | ["gql"; "public"] ->
        None
      | "gql" :: _ ->
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {
                 loc = name.loc;
                 fileUri = env.file.uri;
                 message =
                   Printf.sprintf
                     "`%s` is an invalid @gql annotation. Valid annotations \
                      are `@gql.type` for object types, `@gql.inputObject` for \
                      input objects, `@gql.enum` for enums, `@gql.union` for \
                      unions."
                     name.txt;
               };
        None
      | _ -> None)

let addDirectiveDiagnostic ~(schemaState : schemaState)
    ~(env : SharedTypes.QueryEnv.t) ~loc message =
  schemaState
  |> addDiagnostic ~diagnostic:{loc; fileUri = env.file.uri; message}

let payloadExpressions (payload : Parsetree.payload) =
  match payload with
  | PStr [{pstr_desc = Pstr_eval (expression, _)}] -> (
    match expression.pexp_desc with
    | Pexp_tuple expressions -> expressions
    | _ -> [expression])
  | _ -> []

let rec constValueFromExpression (expression : Parsetree.expression) =
  let open Parsetree in
  match expression.pexp_desc with
  | Pexp_constant (Pconst_integer (value, _)) -> Ok (ConstInt value)
  | Pexp_constant (Pconst_float (value, _)) -> Ok (ConstFloat value)
  | Pexp_constant (Pconst_string (value, _)) -> Ok (ConstString value)
  | Pexp_construct ({txt = Lident "true"}, None)
  | Pexp_ident {txt = Lident "true"} ->
    Ok (ConstBoolean true)
  | Pexp_construct ({txt = Lident "false"}, None)
  | Pexp_ident {txt = Lident "false"} ->
    Ok (ConstBoolean false)
  | Pexp_construct ({txt = Lident ("None" | "null")}, None)
  | Pexp_ident {txt = Lident "null"} ->
    Ok ConstNull
  | Pexp_variant (name, None) -> Ok (ConstEnum name)
  | Pexp_construct ({txt = constructor}, None) | Pexp_ident {txt = constructor}
    ->
    Ok (ConstEnum (Longident.last constructor))
  | Pexp_array values ->
    values
    |> List.fold_left
         (fun result value ->
           match (result, constValueFromExpression value) with
           | Ok values, Ok value -> Ok (value :: values)
           | Error message, _ | _, Error message -> Error message)
         (Ok [])
    |> Result.map (fun values -> ConstList (List.rev values))
  | Pexp_record (fields, None) ->
    fields
    |> List.fold_left
         (fun result (field : Parsetree.expression Parsetree.record_element) ->
           match (result, constValueFromExpression field.x) with
           | Ok fields, Ok value ->
             Ok ((Longident.last field.lid.txt, value) :: fields)
           | Error message, _ | _, Error message -> Error message)
         (Ok [])
    |> Result.map (fun fields -> ConstObject (List.rev fields))
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Lident ("~-" | "~-." | "-")}};
        args = [(_, value)];
      } -> (
    match constValueFromExpression value with
    | Ok (ConstInt value) -> Ok (ConstInt ("-" ^ value))
    | Ok (ConstFloat value) -> Ok (ConstFloat ("-" ^ value))
    | _ -> Error "Only numeric GraphQL constant values can be negated.")
  | _ ->
    Error
      "Expected a GraphQL constant: null, a boolean, number, string, enum, \
       array, or record."

type resolverSourceParameter = {
  label: Asttypes.arg_label;
  loc: Location.t;
  attributes: Parsetree.attributes;
  defaultValue: Parsetree.expression option;
}

let resolverSourceCache : (string, string * Parsetree.structure) Hashtbl.t =
  Hashtbl.create 16

let resolverParametersFromSource ~(env : SharedTypes.QueryEnv.t) ~resolverName
    ~resolverLoc =
  let rec patternName (pattern : Parsetree.pattern) =
    match pattern.ppat_desc with
    | Ppat_var {txt} -> Some txt
    | Ppat_alias (_, {txt}) -> Some txt
    | Ppat_constraint (pattern, _) -> patternName pattern
    | _ -> None
  in
  let contains outer inner =
    outer.Location.loc_start.pos_cnum <= inner.Location.loc_start.pos_cnum
    && outer.loc_end.pos_cnum >= inner.loc_end.pos_cnum
  in
  let rec bindingsOfModuleExpression (expression : Parsetree.module_expr) =
    match expression.pmod_desc with
    | Pmod_structure structure -> bindingsOfStructure structure
    | Pmod_constraint (expression, _) -> bindingsOfModuleExpression expression
    | _ -> []
  and bindingsOfStructure structure =
    structure
    |> List.concat_map (fun (item : Parsetree.structure_item) ->
        match item.pstr_desc with
        | Pstr_value (_, bindings) -> bindings
        | Pstr_module binding -> bindingsOfModuleExpression binding.pmb_expr
        | Pstr_recmodule bindings ->
          bindings
          |> List.concat_map (fun (binding : Parsetree.module_binding) ->
              bindingsOfModuleExpression binding.pmb_expr)
        | _ -> [])
  in
  let rec parametersOfExpression (expression : Parsetree.expression) =
    match expression.pexp_desc with
    | Pexp_fun {arg_label; default; lhs; rhs; _} ->
      {
        label = arg_label;
        loc = lhs.ppat_loc;
        attributes = expression.pexp_attributes @ lhs.ppat_attributes;
        defaultValue = default;
      }
      :: parametersOfExpression rhs
    | Pexp_constraint (expression, _) -> parametersOfExpression expression
    | _ -> []
  in
  let cmtPath = env.file.uri |> Uri.toPath in
  let paths =
    if Filename.check_suffix cmtPath ".resi" then
      [Filename.chop_suffix cmtPath ".resi" ^ ".res"]
    else [cmtPath]
  in
  let parsedSource =
    paths
    |> List.find_map (fun path ->
        match Files.readFile path with
        | None -> None
        | Some source -> (
          try
            let digest = Digest.to_hex (Digest.string source) in
            let structure =
              match Hashtbl.find_opt resolverSourceCache path with
              | Some (cachedDigest, structure) when cachedDigest = digest ->
                structure
              | _ ->
                let {Res_driver.parsetree = structure} =
                  Res_driver.parse_implementation_from_source ~for_printer:true
                    ~source ~display_filename:path
                in
                Hashtbl.replace resolverSourceCache path (digest, structure);
                structure
            in
            Some structure
          with _ -> None))
  in
  match parsedSource with
  | None -> []
  | Some structure -> (
    try
      let bindings =
        structure |> bindingsOfStructure
        |> List.filter (fun (binding : Parsetree.value_binding) ->
            patternName binding.pvb_pat = Some resolverName)
      in
      let matchingBinding =
        match
          bindings
          |> List.find_opt (fun (binding : Parsetree.value_binding) ->
              contains binding.pvb_loc resolverLoc
              || contains binding.pvb_pat.ppat_loc resolverLoc
              || contains resolverLoc binding.pvb_pat.ppat_loc)
        with
        | Some binding -> Some binding
        | None -> (
          match bindings with
          | [binding] -> Some binding
          | _ -> None)
      in
      matchingBinding
      |> Option.map (fun (binding : Parsetree.value_binding) ->
          parametersOfExpression binding.pvb_expr)
      |> Option.value ~default:[]
    with _ -> [])

type directiveConfig = {locations: gqlDirectiveLocation list; repeatable: bool}

let directiveConfigFromAttributes ~schemaState ~(env : SharedTypes.QueryEnv.t)
    attributes =
  let directiveAttributes =
    attributes
    |> List.filter (fun ((name, _) : Parsetree.attribute) ->
           name.txt = "gql.directive")
  in
  (match directiveAttributes with
  | _ :: (name, _) :: _ ->
    addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc
      "Only one `@gql.directive` annotation is allowed."
  | _ -> ());
  directiveAttributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
        let invalid message =
          addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc message;
          Some None
        in
        match payloadExpressions payload with
        | [{pexp_desc = Pexp_record (fields, None)}] -> (
          let fields =
            fields
            |> List.map
                 (fun (field : Parsetree.expression Parsetree.record_element) ->
                   (Longident.last field.lid.txt, field.x))
          in
          let unknownFields =
            fields
            |> List.filter_map (fun (fieldName, _) ->
                if fieldName = "locations" || fieldName = "repeatable" then None
                else Some fieldName)
          in
          if unknownFields <> [] then
            invalid
              (Printf.sprintf
                 "Unknown `@gql.directive` configuration field%s: %s."
                 (if List.length unknownFields = 1 then "" else "s")
                 (String.concat ", " unknownFields))
          else
            let repeatable =
              match List.assoc_opt "repeatable" fields with
              | None -> Ok false
              | Some
                  {
                    pexp_desc =
                      ( Pexp_construct ({txt = Lident "true"}, None)
                      | Pexp_ident {txt = Lident "true"} );
                  } ->
                Ok true
              | Some
                  {
                    pexp_desc =
                      ( Pexp_construct ({txt = Lident "false"}, None)
                      | Pexp_ident {txt = Lident "false"} );
                  } ->
                Ok false
              | Some _ -> Error "`repeatable` must be a boolean literal."
            in
            let locations =
              match List.assoc_opt "locations" fields with
              | Some {pexp_desc = Pexp_array values} ->
                values
                |> List.fold_left
                     (fun result (value : Parsetree.expression) ->
                       match (result, value.pexp_desc) with
                       | ( Ok locations,
                           Pexp_constant (Pconst_string (location, _)) ) -> (
                         match
                           GenerateSchemaDirectiveUtils.locationOfString
                             location
                         with
                         | Some location -> Ok (location :: locations)
                         | None ->
                           Error
                             (Printf.sprintf
                                "`%s` is not a GraphQL directive location."
                                location))
                       | Error message, _ -> Error message
                       | _ ->
                         Error "`locations` must contain only string literals.")
                     (Ok [])
                |> Result.map List.rev
              | Some _ -> Error "`locations` must be an array of strings."
              | None -> Error "`@gql.directive` requires `locations`."
            in
            match (locations, repeatable) with
            | Ok [], _ -> invalid "`@gql.directive` requires a location."
            | Ok locations, Ok repeatable -> Some (Some {locations; repeatable})
            | Error message, _ | _, Error message -> invalid message)
        | _ ->
          invalid
            "`@gql.directive` requires a record payload, for example \
             `@gql.directive({locations: [\"FIELD_DEFINITION\"]})`.")

type schemaConfig = {
  queryTypeName: string option;
  mutationTypeName: string option;
  subscriptionTypeName: string option;
}

let schemaConfigFromAttributes ~schemaState ~(env : SharedTypes.QueryEnv.t)
    attributes =
  attributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
      if name.txt <> "gql.schema" then None
      else
        let invalid message =
          addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc message;
          Some None
        in
        let parseFields fields =
          let fields =
            fields
            |> List.map
                 (fun (field : Parsetree.expression Parsetree.record_element) ->
                   (Longident.last field.lid.txt, field.x))
          in
          let allowed = ["query"; "mutation"; "subscription"] in
          let unknownFields =
            fields
            |> List.filter_map (fun (fieldName, _) ->
                if List.mem fieldName allowed then None else Some fieldName)
          in
          let duplicateFields =
            allowed
            |> List.filter (fun fieldName ->
                fields
                |> List.filter (fun (name, _) -> name = fieldName)
                |> List.length |> ( < ) 1)
          in
          if unknownFields <> [] then
            Error
              (Printf.sprintf "Unknown `@gql.schema` configuration field%s: %s."
                 (if List.length unknownFields = 1 then "" else "s")
                 (String.concat ", " unknownFields))
          else if duplicateFields <> [] then
            Error
              (Printf.sprintf
                 "Duplicate `@gql.schema` configuration field%s: %s."
                 (if List.length duplicateFields = 1 then "" else "s")
                 (String.concat ", " duplicateFields))
          else
            allowed
            |> List.fold_left
                 (fun result fieldName ->
                   match (result, List.assoc_opt fieldName fields) with
                   | Error message, _ -> Error message
                   | Ok values, None -> Ok ((fieldName, None) :: values)
                   | ( Ok values,
                       Some
                         {
                           pexp_desc =
                             Pexp_constant (Pconst_string (typeName, _));
                         } ) ->
                     Ok ((fieldName, Some typeName) :: values)
                   | Ok _, Some _ ->
                     Error
                       (Printf.sprintf
                          "`%s` must be a GraphQL type name string." fieldName))
                 (Ok [])
            |> Result.map (fun values ->
                {
                  queryTypeName = List.assoc "query" values;
                  mutationTypeName = List.assoc "mutation" values;
                  subscriptionTypeName = List.assoc "subscription" values;
                })
        in
        match payloadExpressions payload with
        | [] ->
          Some
            (Some
               {
                 queryTypeName = None;
                 mutationTypeName = None;
                 subscriptionTypeName = None;
               })
        | [{pexp_desc = Pexp_record (fields, None)}] -> (
          match parseFields fields with
          | Ok config -> Some (Some config)
          | Error message -> invalid message)
        | _ ->
          invalid
            "`@gql.schema` takes no payload or a record containing `query`, \
             `mutation`, and `subscription` type-name strings.")

let defaultValueFromAttributes ~schemaState ~(env : SharedTypes.QueryEnv.t)
    attributes =
  let defaults =
    attributes
    |> List.filter (fun ((name, _) : Parsetree.attribute) ->
        name.txt = "gql.default")
  in
  match defaults with
  | [] -> None
  | (name, payload) :: rest -> (
    if rest <> [] then
      addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc
        "Only one `@gql.default` annotation is allowed.";
    match payloadExpressions payload with
    | [expression] -> (
      match constValueFromExpression expression with
      | Ok value -> Some value
      | Error message ->
        addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc message;
        None)
    | _ ->
      addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc
        "`@gql.default` requires exactly one GraphQL constant value.";
      None)

let descriptionFromAttributes ~schemaState ~(env : SharedTypes.QueryEnv.t)
    attributes =
  attributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
      if name.txt <> "gql.description" then None
      else
        match payloadExpressions payload with
        | [{pexp_desc = Pexp_constant (Pconst_string (description, _))}] ->
          Some description
        | _ ->
          addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc
            "`@gql.description` requires a string.";
          None)

let specifiedByUrlFromAttributes ~schemaState ~(env : SharedTypes.QueryEnv.t)
    attributes =
  attributes
  |> List.find_map (fun ((name, payload) : Parsetree.attribute) ->
      if name.txt <> "specifiedBy" then None
      else
        match payloadExpressions payload with
        | [{pexp_desc = Pexp_constant (Pconst_string (url, _))}] -> Some url
        | _ ->
          addDirectiveDiagnostic ~schemaState ~env ~loc:name.loc
            "`@specifiedBy` requires a string URL.";
          None)

let directiveApplicationsFromAttributes ~schemaState
    ~(env : SharedTypes.QueryEnv.t) attributes =
  let parseArgumentFields fields =
    fields
    |> List.fold_left
         (fun result (field : Parsetree.expression Parsetree.record_element) ->
           match (result, constValueFromExpression field.x) with
           | Ok arguments, Ok value ->
             Ok ((Longident.last field.lid.txt, value) :: arguments)
           | Error message, _ | _, Error message -> Error message)
         (Ok [])
    |> Result.map List.rev
  in
  let parseArguments = function
    | None -> Ok []
    | Some {Parsetree.pexp_desc = Pexp_record (fields, None)} ->
      parseArgumentFields fields
    | Some _ -> Error "`args` must be a record of GraphQL constant values."
  in
  attributes
  |> List.filter_map (fun ((attributeName, payload) : Parsetree.attribute) ->
      if attributeName.txt <> "gql.annotate" then None
      else
        let invalid message =
          addDirectiveDiagnostic ~schemaState ~env ~loc:attributeName.loc
            message;
          None
        in
        match payloadExpressions payload with
        | [{pexp_desc = Pexp_constant (Pconst_string (name, _))}] ->
          Some
            {
              name;
              arguments = [];
              loc = attributeName.loc;
              fileUri = env.file.uri;
            }
        | [{pexp_desc = Pexp_record (fields, None)}] -> (
          let fields =
            fields
            |> List.map
                 (fun (field : Parsetree.expression Parsetree.record_element) ->
                   (Longident.last field.lid.txt, field.x))
          in
          let unknownFields =
            fields
            |> List.filter_map (fun (name, _) ->
                if name = "name" || name = "args" then None else Some name)
          in
          if unknownFields <> [] then
            invalid
              (Printf.sprintf
                 "Unknown `@gql.annotate` configuration field%s: %s."
                 (if List.length unknownFields = 1 then "" else "s")
                 (String.concat ", " unknownFields))
          else
            match
              (List.assoc_opt "name" fields, List.assoc_opt "args" fields)
            with
            | ( Some {pexp_desc = Pexp_constant (Pconst_string (name, _))},
                arguments ) -> (
              match parseArguments arguments with
              | Ok arguments ->
                Some
                  {
                    name;
                    arguments;
                    loc = attributeName.loc;
                    fileUri = env.file.uri;
                  }
              | Error message -> invalid message)
            | _ -> invalid "`@gql.annotate` requires a string `name` field.")
        | [
         {pexp_desc = Pexp_constant (Pconst_string (name, _))};
         {pexp_desc = Pexp_record (fields, None)};
        ] -> (
          parseArgumentFields fields |> function
          | Ok arguments ->
            Some
              {
                name;
                arguments;
                loc = attributeName.loc;
                fileUri = env.file.uri;
              }
          | Error message -> invalid message)
        | _ ->
          invalid
            "`@gql.annotate` expects `{name: \"directive\"}` with an optional \
             `args` record of GraphQL constant values.")

let registerDirectiveApplications ~target ~attributes ~schemaState ~env =
  let applications =
    directiveApplicationsFromAttributes ~schemaState ~env attributes
  in
  match applications with
  | [] -> ()
  | applications ->
    let existing =
      Hashtbl.find_opt schemaState.appliedDirectives target
      |> Option.value ~default:[]
    in
    let applications =
      applications
      |> List.filter (fun (application : gqlDirectiveApplication) ->
          existing
          |> List.exists (fun (existingApplication : gqlDirectiveApplication) ->
              existingApplication.loc = application.loc)
          |> not)
    in
    Hashtbl.replace schemaState.appliedDirectives target
      (existing @ applications)

let directivesForTarget (schemaState : schemaState) target =
  Hashtbl.find_opt schemaState.appliedDirectives target
  |> Option.value ~default:[]

let extractGqlImplementsAttributes
    ~(schemaState : GenerateSchemaTypes.schemaState)
    ~(env : SharedTypes.QueryEnv.t) (attributes : Parsetree.attributes) =
  attributes
  |> List.filter_map (fun ((name, payload) : Parsetree.attribute) ->
      match String.split_on_char '.' name.txt with
      | ["gql"; "implements"] -> (
        match payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc =
                          Pexp_constant (Pconst_string (interfaceName, _));
                      },
                      _ );
              };
            ] ->
          Some {interfaceName; loc = name.loc; fileUri = env.file.uri}
        | _ ->
          schemaState
          |> addDiagnostic
               ~diagnostic:
                 {
                   loc = name.loc;
                   fileUri = env.file.uri;
                   message =
                     "`@gql.implements` requires a string literal GraphQL \
                      interface name.";
                 };
          None)
      | _ -> None)

let emptyDeclaredAuthorization : declaredAuthorization =
  {functions = []; public = None}

let authorizationCoordinate ~parentTypeName ~fieldName =
  parentTypeName ^ "." ^ fieldName

let authorizationFunctionName (reference : authorizationFunctionReference) =
  String.concat "." reference.path

let addAuthorizationDiagnostic ~schemaState ~(env : SharedTypes.QueryEnv.t) ~loc
    message =
  schemaState
  |> addDiagnostic
       ~diagnostic:{loc; fileUri = env.SharedTypes.QueryEnv.file.uri; message}

let functionReferenceFromPayload ~schemaState ~(env : SharedTypes.QueryEnv.t)
    ~attributeLoc (payload : Parsetree.payload) =
  match payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval ({pexp_desc = Pexp_ident {txt = functionPath}}, _);
        };
      ] ->
    let path = Longident.flatten functionPath in
    if List.length path < 2 then (
      addAuthorizationDiagnostic ~schemaState ~env ~loc:attributeLoc
        "`@gql.authorize` requires a module-qualified function path, for \
         example `Security.canRead`.";
      None)
    else Some {path; loc = attributeLoc; fileUri = env.file.uri}
  | _ ->
    addAuthorizationDiagnostic ~schemaState ~env ~loc:attributeLoc
      "`@gql.authorize` requires a module-qualified function path, for example \
       `Security.canRead`.";
    None

let publicReasonFromPayload ~schemaState ~(env : SharedTypes.QueryEnv.t)
    ~attributeLoc (payload : Parsetree.payload) =
  let reason =
    match payload with
    | PStr
        [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (fields, None)}, _)}]
      ->
      fields
      |> List.find_map
           (fun (field : Parsetree.expression Parsetree.record_element) ->
             if Longident.last field.lid.txt <> "reason" then None
             else
               match field.x.pexp_desc with
               | Pexp_constant (Pconst_string (reason, _)) -> Some reason
               | _ -> None)
    | _ -> None
  in
  let nonWhitespaceLength reason =
    reason
    |> String.fold_left
         (fun length -> function
           | ' ' | '\t' | '\r' | '\n' -> length
           | _ -> length + 1)
         0
  in
  match reason with
  | Some reason when nonWhitespaceLength reason >= 3 ->
    Some {reason; loc = attributeLoc; fileUri = env.file.uri}
  | _ ->
    addAuthorizationDiagnostic ~schemaState ~env ~loc:attributeLoc
      "`@gql.public` requires a reason with at least 3 non-whitespace \
       characters, for example `@gql.public({reason: \"Public profile \
       data\"})`.";
    None

let extractDeclaredAuthorization ~allowPublic ~schemaState
    ~(env : SharedTypes.QueryEnv.t) (attributes : Parsetree.attributes) =
  attributes
  |> List.fold_left
       (fun (declared : declaredAuthorization)
            ((name, payload) : Parsetree.attribute) ->
         match String.split_on_char '.' name.txt with
         | ["gql"; "authorize"] -> (
           match
             functionReferenceFromPayload ~schemaState ~env
               ~attributeLoc:name.loc payload
           with
           | None -> declared
           | Some fn -> {declared with functions = declared.functions @ [fn]})
         | ["gql"; "public"] -> (
           if not allowPublic then (
             addAuthorizationDiagnostic ~schemaState ~env ~loc:name.loc
               "`@gql.public` can only be used on output fields or resolver \
                functions, not on a type.";
             declared)
           else
             match
               publicReasonFromPayload ~schemaState ~env ~attributeLoc:name.loc
                 payload
             with
             | None -> declared
             | Some public -> (
               match declared.public with
               | None -> {declared with public = Some public}
               | Some _ ->
                 addAuthorizationDiagnostic ~schemaState ~env ~loc:name.loc
                   "Only one `@gql.public` annotation is allowed per field.";
                 declared))
         | _ -> declared)
       emptyDeclaredAuthorization

let registerAuthorizationDeclaration ~coordinate
    (declared : declaredAuthorization) ~(schemaState : schemaState) =
  if declared.functions = [] && Option.is_none declared.public then ()
  else
    match Hashtbl.find_opt schemaState.authorizationDeclarations coordinate with
    | None ->
      Hashtbl.replace schemaState.authorizationDeclarations coordinate declared
    | Some existing ->
      Hashtbl.replace schemaState.authorizationDeclarations coordinate
        {
          functions =
            existing.functions
            @ (declared.functions
              |> List.filter (fun reference ->
                  not (List.mem reference existing.functions)));
          public =
            (match (existing.public, declared.public) with
            | Some public, _ -> Some public
            | None, public -> public);
        }

let registerAuthorizationAttributes ~coordinate ~allowPublic ~attributes
    ~schemaState ~env =
  attributes
  |> extractDeclaredAuthorization ~allowPublic ~schemaState ~env
  |> registerAuthorizationDeclaration ~coordinate ~schemaState

let getFieldAttribute gqlAttribute =
  match gqlAttribute with
  | Some Field -> Some gqlAttribute
  | _ -> None

let getFieldAttributeFromRawAttributes ~env ~schemaState attributes =
  attributes |> extractGqlAttribute ~env ~schemaState |> getFieldAttribute

type expectedType =
  | ObjectType
  | InputObject
  | Field
  | Enum
  | Union
  | InputUnion
  | Interface
  | Scalar
  | ScalarT of {modulePath: string list}

(** Figures out the path to a target type in a file. *)
let rec findModulePathOfType ~schemaState ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : expectedType) ?(modulePath = [])
    ?(structure = env.file.structure) name =
  let open SharedTypes.Module in
  structure.items
  |> List.find_map (fun (item : item) ->
      match
        ( item.kind,
          expectedType,
          item.attributes |> extractGqlAttribute ~env ~schemaState )
      with
      | Type ({kind = Variant _}, _), Enum, Some Enum when item.name = name ->
        Some modulePath
      | Type ({kind = Variant _}, _), Union, Some Union when item.name = name ->
        Some modulePath
      | Type ({kind = Variant _}, _), InputUnion, Some InputUnion
        when item.name = name ->
        Some modulePath
      | Type ({kind = Record _}, _), ObjectType, Some ObjectType
        when item.name = name ->
        Some modulePath
      | ( Type
            ( {
                name = "query" | "mutation" | "subscription";
                kind = Abstract None;
              },
              _ ),
          ObjectType,
          Some ObjectType )
        when item.name = name ->
        Some modulePath
      | Type ({kind = Record _}, _), Interface, Some Interface
        when item.name = name ->
        Some modulePath
      | Type ({kind = Record _}, _), InputObject, Some InputObject
        when item.name = name ->
        Some modulePath
      | Type ({kind = Abstract (Some _)}, _), Scalar, Some Scalar
        when item.name = name ->
        Some modulePath
      | ( Module
            {
              type_ =
                Constraint (Structure _implStructure, Structure intfStructure);
              _;
            },
          _,
          _ ) ->
        name
        |> findModulePathOfType ~env ~expectedType
             ~modulePath:(intfStructure.name :: modulePath)
             ~structure:intfStructure ~schemaState
      | Module {type_ = Structure structure; _}, _, _ ->
        name
        |> findModulePathOfType ~env ~expectedType
             ~modulePath:(structure.name :: modulePath)
             ~structure ~schemaState
      | _ -> None)

let findTypeLocation ~(env : SharedTypes.QueryEnv.t)
    ~(expectedType : expectedType) ~schemaState ~loc name =
  let typeLocation : typeLocationLoc =
    {
      fileName = env.file.moduleName;
      modulePath = [];
      typeName = name;
      loc;
      fileUri = env.file.uri;
    }
  in
  match findModulePathOfType ~schemaState ~env ~expectedType name with
  | None ->
    schemaState
    |> addDiagnostic
         ~diagnostic:
           {
             loc;
             fileUri = env.file.uri;
             message =
               Printf.sprintf
                 "Could not determine location of %s backed by type `%s` in %s."
                 (match expectedType with
                 | Union -> "union"
                 | Enum -> "enum"
                 | ObjectType -> "object type"
                 | InputObject -> "input object"
                 | InputUnion -> "input union"
                 | Field -> "field"
                 | Interface -> "interface"
                 | Scalar | ScalarT _ -> "scalar")
                 name env.file.moduleName;
           };

    typeLocation
  | Some modulePath -> {typeLocation with modulePath}

let typeLocationToAccessor (typeLocation : typeLocationLoc) =
  [typeLocation.fileName] @ typeLocation.modulePath @ [typeLocation.typeName]
  |> String.concat "."

let typeLocationModuleToAccesor (typeLocation : typeLocationLoc) endingPath =
  [typeLocation.fileName] @ typeLocation.modulePath @ endingPath
  |> String.concat "."

let capitalizeFirstChar s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s

let uncapitalizeFirstChar s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.lowercase_ascii c else c) s

let noticeObjectType ~env ~loc ~schemaState ~displayName ?syntheticTypeLocation
    ?description ?(ignoreTypeLocation = false) ?(explicitInterfaces = [])
    ~makeFields typeName =
  if Hashtbl.mem schemaState.types typeName then ()
  else
    (*Printf.printf "noticing %s\n" typeName;*)
    let typ : gqlObjectType =
      {
        id = typeName;
        displayName;
        syntheticTypeLocation;
        fields = makeFields ();
        description;
        interfaces = [];
        explicitInterfaces;
        typeLocation =
          (if ignoreTypeLocation then None
           else
             Some
               (Concrete
                  (findTypeLocation ~schemaState typeName ~env ~loc
                     ~expectedType:ObjectType)));
      }
    in
    Hashtbl.add schemaState.types typeName typ;
    match typeName with
    | "query" -> schemaState.query <- Some typ
    | "mutation" -> schemaState.mutation <- Some typ
    | "subscription" -> schemaState.subscription <- Some typ
    | _ -> ()

let addEnum id ~(makeEnum : unit -> gqlEnum) ~debug ~schemaState =
  if Hashtbl.mem schemaState.enums id then ()
  else (
    if debug then Printf.printf "Adding enum %s\n" id;
    Hashtbl.replace schemaState.enums id (makeEnum ()))

let addUnion id ~(makeUnion : unit -> gqlUnion) ~debug ~schemaState =
  if Hashtbl.mem schemaState.unions id then ()
  else (
    if debug then Printf.printf "Adding union %s\n" id;
    Hashtbl.replace schemaState.unions id (makeUnion ()))

let addInputUnion id ~(makeInputUnion : unit -> gqlInputUnionType) ~debug
    ~schemaState =
  if Hashtbl.mem schemaState.unions id then ()
  else (
    if debug then Printf.printf "Adding input union %s\n" id;
    Hashtbl.replace schemaState.inputUnions id (makeInputUnion ()))

let addScalar ~debug ~schemaState ?description ?specifiedByUrl ~typeLocation
    ?encoderDecoderLoc ?(hasParseLiteral = false) id =
  if Hashtbl.mem schemaState.scalars id then ()
  else (
    if debug then Printf.printf "Adding scalar %s\n" id;
    Hashtbl.replace schemaState.scalars id
      {
        id;
        displayName = capitalizeFirstChar id;
        description;
        typeLocation;
        specifiedByUrl;
        encoderDecoderLoc;
        hasParseLiteral;
      })

let addInterface id ~(makeInterface : unit -> gqlInterface) ~debug ~schemaState
    =
  if Hashtbl.mem schemaState.interfaces id then ()
  else (
    if debug then Printf.printf "Adding interface %s\n" id;
    Hashtbl.replace schemaState.interfaces id (makeInterface ()))

let addInputObject id ~(makeInputObject : unit -> gqlInputObjectType) ~debug
    ~schemaState =
  if Hashtbl.mem schemaState.inputObjects id then ()
  else (
    if debug then Printf.printf "Adding input object %s\n" id;
    Hashtbl.replace schemaState.inputObjects id (makeInputObject ()))

let addFieldToObjectType ~env ~loc ~field ~schemaState typeName =
  let typ : gqlObjectType =
    match Hashtbl.find_opt schemaState.types typeName with
    | None ->
      {
        id = typeName;
        syntheticTypeLocation = None;
        displayName = capitalizeFirstChar typeName;
        fields = [field];
        interfaces = [];
        explicitInterfaces = [];
        description = field.description;
        typeLocation =
          Some
            (Concrete
               (findTypeLocation ~schemaState typeName ~env ~loc
                  ~expectedType:ObjectType));
      }
    | Some typ -> {typ with fields = field :: typ.fields}
  in
  Hashtbl.replace schemaState.types typeName typ

let addFieldToInterfaceType ~env ~loc ~field ~schemaState typeName =
  let typ : gqlInterface =
    match Hashtbl.find_opt schemaState.interfaces typeName with
    | None ->
      {
        id = typeName;
        displayName = capitalizeFirstChar typeName;
        fields = [field];
        interfaces = [];
        explicitInterfaces = [];
        description = field.description;
        typeLocation =
          findTypeLocation ~schemaState typeName ~env ~loc
            ~expectedType:Interface;
      }
    | Some typ -> {typ with fields = field :: typ.fields}
  in
  Hashtbl.replace schemaState.interfaces typeName typ

let undefinedOrValueAsString ?(escape = false) v =
  match v with
  | None -> "?(None)"
  | Some v -> Printf.sprintf "\"%s\"" (if escape then Json.escape v else v)

let escapeDescriptionForRescript value =
  let escaped = Json.escape value in
  let length = String.length escaped in
  let buffer = Buffer.create (length + 16) in
  let rec append index =
    if index >= length then ()
    else if
      index + 5 < length
      && escaped.[index] = '\\'
      && escaped.[index + 1] = '"'
      && escaped.[index + 2] = '\\'
      && escaped.[index + 3] = '"'
      && escaped.[index + 4] = '\\'
      && escaped.[index + 5] = '"'
    then (
      Buffer.add_string buffer "\\u0022\\u0022\\u0022";
      append (index + 6))
    else (
      Buffer.add_char buffer escaped.[index];
      append (index + 1))
  in
  append 0;
  Buffer.contents buffer

let descriptionAsString = function
  | None -> "?(None)"
  | Some description ->
    Printf.sprintf "\"%s\"" (escapeDescriptionForRescript description)

let trimString str =
  let isSpace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  let len = String.length str in
  let rec find_start i =
    if i >= len then len
    else if not (isSpace str.[i]) then i
    else find_start (i + 1)
  in
  let rec find_end i =
    if i < 0 then 0
    else if not (isSpace str.[i]) then i + 1
    else find_end (i - 1)
  in
  let start = find_start 0 in
  let stop = find_end (len - 1) in
  if start >= stop then "" else String.sub str start (stop - start)

let attributesToDocstring attributes =
  match ProcessAttributes.findDocAttribute attributes with
  | None -> None
  | Some doc -> Some (doc |> trimString)

(** Pulls out name from `as` attribute. *)
let nameFromAttribute (attributes : Parsetree.attributes) ~default =
  match
    attributes
    |> List.find_map (fun (attr : Parsetree.attribute) ->
        match attr with
        | ( {Location.txt = "as"},
            PStr
              [
                {
                  pstr_desc =
                    Pstr_eval
                      ({pexp_desc = Pexp_constant (Pconst_string (name, _))}, _);
                };
              ] ) ->
          Some name
        | _ -> None)
  with
  | Some name -> name
  | None -> default

let findContextArgName (args : gqlArg list) =
  args
  |> List.find_map (fun (arg : gqlArg) ->
      match arg.typ with
      | InjectContext -> Some arg.name
      | _ -> None)

let findInfoArgName (args : gqlArg list) =
  args
  |> List.find_map (fun (arg : gqlArg) ->
      match arg.typ with
      | InjectInfo -> Some arg.name
      | _ -> None)

let findInterfaceTypeArgName (args : gqlArg list) =
  args
  |> List.find_map (fun (arg : gqlArg) ->
      match arg.typ with
      | InjectInterfaceTypename _ -> Some arg.name
      | _ -> None)

let rec typeNeedsConversion (graphqlType : graphqlType) =
  match graphqlType with
  | List inner ->
    (* Lists might need conversion depending on the contents. *)
    typeNeedsConversion inner
  | Nullable _ | RescriptNullable _
  (* Input objects might need conversion because they might have null-enabled fields *)
  | GraphQLInputObject _ | GraphQLInputUnion _ ->
    true
  | _ -> false

let inputUnionToInputObj (inputUnion : gqlInputUnionType) : gqlInputObjectType =
  {
    typeLocation = Some inputUnion.typeLocation;
    syntheticTypeLocation = None;
    description = inputUnion.description;
    displayName = inputUnion.displayName;
    id = inputUnion.id;
    fields =
      inputUnion.members
      |> List.map (fun (iu : gqlInputUnionMember) ->
          {
            name = uncapitalizeFirstChar iu.constructorName;
            resolverStyle = Property iu.fieldName;
            typ = Nullable iu.typ;
            args = [];
            defaultValue = None;
            deprecationReason = None;
            description = iu.description;
            loc = iu.loc;
            fileName = inputUnion.typeLocation.fileName;
            fileUri = inputUnion.typeLocation.fileUri;
            onType = None;
            inheritedFromInterface = None;
          });
  }

(* Runtime conversion for a structure of GraphQL types. *)
let rec generateConverter lastValue (graphqlType : graphqlType) =
  match graphqlType with
  | Nullable inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf
        "(switch %s->Nullable.toOption { | None => None | Some(v) => %s->Some})"
        lastValue innerConverter
    else Printf.sprintf "(%s->Nullable.toOption)" lastValue
  | List inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf "(%s->Array.map(v => %s))" lastValue innerConverter
    else lastValue
  | RescriptNullable inner ->
    if typeNeedsConversion inner then
      let innerConverter = generateConverter "v" inner in
      Printf.sprintf "(%s->Nullable.map(v => %s))" lastValue innerConverter
    else lastValue
  | GraphQLInputObject {displayName} ->
    Printf.sprintf
      "%s->applyConversionToInputObject(input_%s_conversionInstructions)"
      lastValue displayName
  | GraphQLInputUnion {displayName; inlineRecords; emptyPayloads} ->
    (* TODO: Precompute/persist? *)
    Printf.sprintf
      "%s->applyConversionToInputObject(inputUnion_%s_conversionInstructions)->inputUnionUnwrapper([%s], \
       [%s])"
      lastValue displayName
      (inlineRecords
      |> List.map (fun s -> "\"" ^ s ^ "\"")
      |> String.concat ", ")
      (emptyPayloads
      |> List.map (fun s -> "\"" ^ s ^ "\"")
      |> String.concat ", ")
  | _ -> lastValue

let printConversionInstructions name fields =
  let conversions =
    fields
    |> List.filter_map (fun (field : gqlField) ->
        let converter = generateConverter "v" field.typ in
        if converter = "v" then None else Some (field.name, converter))
  in
  let writer = CodeWriter.create 512 in
  if List.length conversions = 0 then
    CodeWriter.add writer (name ^ "->Array.pushMany([])")
  else (
    CodeWriter.line writer (name ^ "->Array.pushMany([");
    CodeWriter.indented writer (fun () ->
        conversions
        |> List.iter (fun (fieldName, converter) ->
            CodeWriter.line writer "(";
            CodeWriter.indented writer (fun () ->
                CodeWriter.line writer (Printf.sprintf "\"%s\"," fieldName);
                CodeWriter.line writer
                  (Printf.sprintf "makeInputObjectFieldConverterFn((v) => %s)"
                     converter));
            CodeWriter.line writer "),"));
    CodeWriter.add writer "])");
  CodeWriter.contents writer

let printInputObjectAssets (inputObject : gqlInputObjectType) =
  printConversionInstructions
    ("input_" ^ inputObject.displayName ^ "_conversionInstructions")
    inputObject.fields

let printInputUnionAssets (inputUnion : gqlInputUnionType) =
  let inputObject = inputUnionToInputObj inputUnion in
  printConversionInstructions
    ("inputUnion_" ^ inputObject.displayName ^ "_conversionInstructions")
    inputObject.fields

let printDiagnostic (diagnostic : diagnostic) =
  Printf.sprintf
    "      {\n\
    \        \"range\": %s,\n\
    \        \"file\": \"%s\",\n\
    \        \"message\": \"%s\"\n\
    \      }"
    (diagnostic.loc |> Utils.cmtLocToRange |> Protocol.stringifyRange)
    (diagnostic.fileUri |> Uri.toPath |> Json.escape)
    (diagnostic.message |> Json.escape)

let pathIdentToList (p : Path.t) =
  let rec pathIdentToListInner ?(acc = []) (p : Path.t) =
    match p with
    | Pident {name} -> name :: acc
    | Pdot (nextPath, id, _) -> [id] @ pathIdentToListInner ~acc nextPath
    | Papply _ -> acc
  in
  let lst = pathIdentToListInner p in
  lst |> List.rev

type positionItemType = ObjectType | Interface

type positionToRead = {
  id: string;
  position: Pos.t * Pos.t;
  typ: positionItemType;
}

type implementsInterface = {
  id: string;
  interfaces: string list;
  typ: positionItemType;
}

let spreadRegexp = Str.regexp_string "..."

let hasSpreadText str =
  try
    let _ = Str.search_forward spreadRegexp str 0 in
    true
  with Not_found -> false

let hashtblToListAlphabetically hashtbl =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) hashtbl []
  |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)

let iterHashtblAlphabetically fn hashtbl =
  hashtblToListAlphabetically hashtbl |> List.iter (fun (k, v) -> fn k v)

let findInterfaceByName (schemaState : schemaState) interfaceName =
  match Hashtbl.find_opt schemaState.interfaces interfaceName with
  | Some intf -> Some intf
  | None ->
    Hashtbl.fold
      (fun _id (intf : gqlInterface) acc ->
        match acc with
        | Some _ -> acc
        | None -> if intf.displayName = interfaceName then Some intf else None)
      schemaState.interfaces None

let appendUniqueString values value =
  if List.mem value values then values else values @ [value]

let appendUniqueStrings values newValues =
  newValues |> List.fold_left appendUniqueString values

let isResolverField (field : gqlField) =
  match field.resolverStyle with
  | Resolver _ -> true
  | Property _ -> false

let addMissingInterfaceResolverFields ~displayName fields (intf : gqlInterface)
    =
  let hasField name =
    fields |> List.exists (fun (field : gqlField) -> field.name = name)
  in
  fields
  @ (intf.fields
    |> List.filter (fun (field : gqlField) ->
        isResolverField field && hasField field.name = false)
    |> List.map (fun (field : gqlField) ->
        {
          field with
          onType = Some displayName;
          inheritedFromInterface = Some intf.displayName;
        }))

let inheritInterfaceResolverFields (schemaState : schemaState) =
  schemaState.types |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlObjectType)) ->
      let fields =
        typ.interfaces
        |> List.fold_left
             (fun fields intfId ->
               match Hashtbl.find_opt schemaState.interfaces intfId with
               | None -> fields
               | Some intf ->
                 addMissingInterfaceResolverFields ~displayName:typ.displayName
                   fields intf)
             typ.fields
      in
      Hashtbl.replace schemaState.types typ.id {typ with fields});

  schemaState.interfaces |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlInterface)) ->
      let fields =
        typ.interfaces
        |> List.fold_left
             (fun fields intfId ->
               match Hashtbl.find_opt schemaState.interfaces intfId with
               | None -> fields
               | Some intf ->
                 addMissingInterfaceResolverFields ~displayName:typ.displayName
                   fields intf)
             typ.fields
      in
      Hashtbl.replace schemaState.interfaces typ.id {typ with fields})

let implementedById (implementedBy : interfaceImplementedBy) =
  match implementedBy with
  | ObjectType typ -> typ.id
  | Interface intf -> intf.id

let registerInterfaceImplementedBy processedSchema intfId
    (implementedBy : interfaceImplementedBy) =
  match Hashtbl.find_opt processedSchema.interfaceImplementedBy intfId with
  | None ->
    Hashtbl.add processedSchema.interfaceImplementedBy intfId [implementedBy]
  | Some existing ->
    if
      existing
      |> List.exists (fun existingImplementedBy ->
          implementedById existingImplementedBy = implementedById implementedBy)
    then ()
    else
      Hashtbl.replace processedSchema.interfaceImplementedBy intfId
        (implementedBy :: existing)

let rec collectInterfaceAncestors (schemaState : schemaState) ~visited intfId =
  if List.mem intfId visited then []
  else
    match Hashtbl.find_opt schemaState.interfaces intfId with
    | None -> []
    | Some intf ->
      let visited = intfId :: visited in
      intf.interfaces
      |> List.fold_left
           (fun acc parentInterfaceId ->
             let acc = appendUniqueString acc parentInterfaceId in
             appendUniqueStrings acc
               (collectInterfaceAncestors schemaState ~visited parentInterfaceId))
           []

let expandInterfaceList schemaState interfaces =
  interfaces
  |> List.fold_left
       (fun acc intfId ->
         let acc = appendUniqueString acc intfId in
         appendUniqueStrings acc
           (collectInterfaceAncestors schemaState ~visited:[] intfId))
       []

let expandTransitiveInterfaceImplementations schemaState processedSchema =
  schemaState.types |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlObjectType)) ->
      let expandedInterfaces = expandInterfaceList schemaState typ.interfaces in
      let typ = {typ with interfaces = expandedInterfaces} in
      Hashtbl.replace schemaState.types typ.id typ;
      expandedInterfaces
      |> List.iter (fun intfId ->
          registerInterfaceImplementedBy processedSchema intfId (ObjectType typ)));

  schemaState.interfaces |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlInterface)) ->
      let expandedInterfaces = expandInterfaceList schemaState typ.interfaces in
      let typ = {typ with interfaces = expandedInterfaces} in
      Hashtbl.replace schemaState.interfaces typ.id typ;
      expandedInterfaces
      |> List.iter (fun intfId ->
          registerInterfaceImplementedBy processedSchema intfId (Interface typ)))

let uniqueImplementedBy implementedBy =
  implementedBy
  |> List.fold_left
       (fun acc item ->
         if
           acc
           |> List.exists (fun existingItem ->
               implementedById existingItem = implementedById item)
         then acc
         else item :: acc)
       []
  |> List.rev

let finalizeConcreteInterfaceImplementations (processedSchema : processedSchema)
    =
  let originalImplementations =
    Hashtbl.copy processedSchema.interfaceImplementedBy
  in
  let rec collectConcreteImplementations ~visited intfId =
    if List.mem intfId visited then []
    else
      match Hashtbl.find_opt originalImplementations intfId with
      | None -> []
      | Some implementedBy ->
        implementedBy
        |> List.fold_left
             (fun acc (implementedBy : interfaceImplementedBy) ->
               match implementedBy with
               | ObjectType _ -> implementedBy :: acc
               | Interface intf ->
                 collectConcreteImplementations ~visited:(intfId :: visited)
                   intf.id
                 @ acc)
             []
        |> uniqueImplementedBy
  in
  originalImplementations
  |> iterHashtblAlphabetically (fun intfId _ ->
      match collectConcreteImplementations ~visited:[] intfId with
      | [] -> Hashtbl.remove processedSchema.interfaceImplementedBy intfId
      | concreteImplementations ->
        Hashtbl.replace processedSchema.interfaceImplementedBy intfId
          concreteImplementations)

let typeLocationLocOfObjectType (typ : gqlObjectType) =
  match typ.typeLocation with
  | Some (Concrete typeLocation) -> Some typeLocation
  | Some (Synthetic {fileUri; modulePath; fileName}) ->
    let loc =
      match typ.syntheticTypeLocation with
      | Some {loc} -> loc
      | None -> Location.none
    in
    Some {fileName; fileUri; modulePath; typeName = typ.id; loc}
  | None -> None

let validateInterfaceImplementations (schemaState : schemaState) =
  schemaState.types |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlObjectType)) ->
      match typeLocationLocOfObjectType typ with
      | None -> ()
      | Some typeLocation ->
        typ.interfaces
        |> List.iter (fun intfId ->
            match Hashtbl.find_opt schemaState.interfaces intfId with
            | None -> ()
            | Some intf ->
              GenerateSchemaValidation.validateInterfaceImplementation
                ~schemaState ~loc:typeLocation.loc ~fileUri:typeLocation.fileUri
                ~implementingTypeName:typ.displayName
                ~implementingFields:typ.fields ~interface:intf));
  schemaState.interfaces |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlInterface)) ->
      typ.interfaces
      |> List.iter (fun intfId ->
          match Hashtbl.find_opt schemaState.interfaces intfId with
          | None -> ()
          | Some intf ->
            GenerateSchemaValidation.validateInterfaceImplementation
              ~schemaState ~loc:typ.typeLocation.loc
              ~fileUri:typ.typeLocation.fileUri
              ~implementingTypeName:typ.displayName
              ~implementingFields:typ.fields ~interface:intf))

let resolveSchemaRootTypes (schemaState : schemaState) =
  let findObjectType typeName =
    schemaState.types |> hashtblToListAlphabetically
    |> List.find_map (fun (id, (typ : gqlObjectType)) ->
        if id = typeName || typ.displayName = typeName then Some typ else None)
  in
  let resolve ~operation ~configuredName ~conventionalId current =
    let typeName = Option.value configuredName ~default:conventionalId in
    match findObjectType typeName with
    | Some typ -> Some typ
    | None when Option.is_none configuredName -> current
    | None ->
      let loc, fileUri =
        match schemaState.schemaDefinition with
        | Some definition -> (definition.loc, definition.fileUri)
        | None -> (Location.none, schemaState.rootFileUri)
      in
      schemaState
      |> addDiagnostic
           ~diagnostic:
             {
               loc;
               fileUri;
               message =
                 Printf.sprintf
                   "The `@gql.schema` %s root maps to `%s`, but no GraphQL \
                    object type with that name exists."
                   operation typeName;
             };
      None
  in
  let queryTypeName, mutationTypeName, subscriptionTypeName =
    match schemaState.schemaDefinition with
    | None -> (None, None, None)
    | Some definition ->
      ( definition.queryTypeName,
        definition.mutationTypeName,
        definition.subscriptionTypeName )
  in
  schemaState.query <-
    resolve ~operation:"query" ~configuredName:queryTypeName
      ~conventionalId:"query" schemaState.query;
  schemaState.mutation <-
    resolve ~operation:"mutation" ~configuredName:mutationTypeName
      ~conventionalId:"mutation" schemaState.mutation;
  schemaState.subscription <-
    resolve ~operation:"subscription" ~configuredName:subscriptionTypeName
      ~conventionalId:"subscription" schemaState.subscription

let processSchema (schemaState : schemaState) =
  let processedSchema = {interfaceImplementedBy = Hashtbl.create 10} in
  let positionsToRead = Hashtbl.create 10 in

  resolveSchemaRootTypes schemaState;

  (* Figure out all files that needs reading to check for interface spreads *)
  schemaState.types
  |> Hashtbl.iter (fun _name (t : gqlObjectType) ->
      match t.typeLocation with
      | None -> ()
      | Some typeLocation -> (
        let fileUri =
          (match typeLocation with
            | Synthetic {fileUri} | Concrete {fileUri} -> fileUri)
          |> Uri.toPath
        in
        match Hashtbl.find_opt positionsToRead fileUri with
        | None ->
          Hashtbl.add positionsToRead fileUri
            [
              {
                id = t.id;
                position =
                  (match typeLocation with
                  | Synthetic _ ->
                    (Location.none |> Loc.start, Location.none |> Loc.end_)
                  | Concrete {loc} -> (loc |> Loc.start, loc |> Loc.end_));
                typ = ObjectType;
              };
            ]
        | Some existingEntries ->
          Hashtbl.replace positionsToRead fileUri
            ({
               id = t.id;
               position =
                 (match typeLocation with
                 | Synthetic _ ->
                   (Location.none |> Loc.start, Location.none |> Loc.end_)
                 | Concrete {loc} -> (loc |> Loc.start, loc |> Loc.end_));
               typ = ObjectType;
             }
            :: existingEntries)));

  schemaState.interfaces
  |> Hashtbl.iter (fun _name (t : gqlInterface) ->
      let fileUri = t.typeLocation.fileUri |> Uri.toPath in
      match Hashtbl.find_opt positionsToRead fileUri with
      | None ->
        Hashtbl.add positionsToRead fileUri
          [
            {
              id = t.id;
              position =
                (t.typeLocation.loc |> Loc.start, t.typeLocation.loc |> Loc.end_);
              typ = Interface;
            };
          ]
      | Some existingEntries ->
        Hashtbl.replace positionsToRead fileUri
          ({
             id = t.id;
             position =
               (t.typeLocation.loc |> Loc.start, t.typeLocation.loc |> Loc.end_);
             typ = Interface;
           }
          :: existingEntries));

  (* Read all noted positions so we can check them for spreads. *)
  positionsToRead
  |> Hashtbl.iter (fun fileUri entries ->
      let entries =
        entries
        |> List.sort (fun (a : positionToRead) (b : positionToRead) ->
            compare (a.position |> fst) (b.position |> fst))
      in
      let fileChannel = open_in fileUri in
      let rec loop ?(hasSpread = false) lineNumber acc
          ({position = (startLine, startCol), (endLine, endCol)} as entry) =
        try
          let line = input_line fileChannel in
          if lineNumber > endLine then
            (String.concat "\n" (List.rev acc), lineNumber + 1, hasSpread)
          else if lineNumber >= startLine && lineNumber <= endLine then
            loop
              ~hasSpread:(hasSpread || hasSpreadText line)
              (lineNumber + 1) (line :: acc) entry
          else if lineNumber = startLine then
            let start = if startLine = endLine then startCol else 0 in
            let part = String.sub line start (endCol - start) in
            loop
              ~hasSpread:(hasSpread || hasSpreadText line)
              (lineNumber + 1) (part :: acc) entry
          else
            loop
              ~hasSpread:(hasSpread || hasSpreadText line)
              (lineNumber + 1) acc entry
        with End_of_file ->
          close_in fileChannel;
          (String.concat "\n" (List.rev acc), lineNumber + 1, hasSpread)
      in
      let lastEndlingLine = ref 0 in
      entries
      |> List.iter (fun entry ->
          let typeStr, endingLineNum, hasSpread =
            loop !lastEndlingLine [] entry
          in
          (* Check for interfaces *)
          (if hasSpread then
             match findInterfacesOfType ~schemaState typeStr with
             | None -> ()
             | Some implementsInterfaces -> (
               (* Add all found interfaces to relevant types or interfaces. *)
               match entry.typ with
               | ObjectType ->
                 Hashtbl.replace schemaState.types entry.id
                   {
                     (Hashtbl.find schemaState.types entry.id) with
                     interfaces =
                       appendUniqueStrings
                         (Hashtbl.find schemaState.types entry.id).interfaces
                         implementsInterfaces;
                   };

                 (* Process each interface for this type *)
                 implementsInterfaces
                 |> List.iter (fun intfId ->
                     let interface =
                       Hashtbl.find schemaState.interfaces intfId
                     in
                     let typ = Hashtbl.find schemaState.types entry.id in
                     let doesNotHaveField name =
                       typ.fields
                       |> List.exists (fun (field : gqlField) ->
                           field.name = name)
                       = false
                     in
                     (* Add relevant fields from interface to the type implementing it *)
                     Hashtbl.replace schemaState.types entry.id
                       {
                         typ with
                         fields =
                           typ.fields
                           @ (interface.fields
                             |> List.filter (fun (field : gqlField) ->
                                 doesNotHaveField field.name)
                             |> List.map (fun (field : gqlField) ->
                                 {
                                   field with
                                   onType = Some typ.displayName;
                                   inheritedFromInterface =
                                     Some interface.displayName;
                                 }));
                       };

                     (* Map interface as implemented by this type *)
                     registerInterfaceImplementedBy processedSchema intfId
                       (ObjectType (Hashtbl.find schemaState.types entry.id)))
               | Interface ->
                 Hashtbl.replace schemaState.interfaces entry.id
                   {
                     (Hashtbl.find schemaState.interfaces entry.id) with
                     interfaces =
                       appendUniqueStrings
                         (Hashtbl.find schemaState.interfaces entry.id)
                           .interfaces implementsInterfaces;
                   };
                 implementsInterfaces
                 |> List.iter (fun intfId ->
                     registerInterfaceImplementedBy processedSchema intfId
                       (Interface (Hashtbl.find schemaState.interfaces entry.id)))
               ));
          lastEndlingLine := endingLineNum);
      close_in fileChannel);

  schemaState.types |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlObjectType)) ->
      typ.explicitInterfaces
      |> List.iter (fun {interfaceName; loc; fileUri} ->
          match findInterfaceByName schemaState interfaceName with
          | None ->
            schemaState
            |> addDiagnostic
                 ~diagnostic:
                   {
                     loc;
                     fileUri;
                     message =
                       Printf.sprintf
                         "`%s` declares @gql.implements(\"%s\"), but no \
                          @gql.interface named `%s` exists."
                         typ.displayName interfaceName interfaceName;
                   }
          | Some intf ->
            let currentTyp = Hashtbl.find schemaState.types typ.id in
            Hashtbl.replace schemaState.types typ.id
              {
                currentTyp with
                interfaces = appendUniqueString currentTyp.interfaces intf.id;
              };
            registerInterfaceImplementedBy processedSchema intf.id
              (ObjectType (Hashtbl.find schemaState.types typ.id))));

  schemaState.interfaces |> hashtblToListAlphabetically
  |> List.iter (fun (_id, (typ : gqlInterface)) ->
      typ.explicitInterfaces
      |> List.iter (fun {interfaceName; loc; fileUri} ->
          match findInterfaceByName schemaState interfaceName with
          | None ->
            schemaState
            |> addDiagnostic
                 ~diagnostic:
                   {
                     loc;
                     fileUri;
                     message =
                       Printf.sprintf
                         "`%s` declares @gql.implements(\"%s\"), but no \
                          @gql.interface named `%s` exists."
                         typ.displayName interfaceName interfaceName;
                   }
          | Some intf ->
            let currentTyp = Hashtbl.find schemaState.interfaces typ.id in
            Hashtbl.replace schemaState.interfaces typ.id
              {
                currentTyp with
                interfaces = appendUniqueString currentTyp.interfaces intf.id;
              };
            registerInterfaceImplementedBy processedSchema intf.id
              (Interface (Hashtbl.find schemaState.interfaces typ.id))));

  expandTransitiveInterfaceImplementations schemaState processedSchema;
  inheritInterfaceResolverFields schemaState;
  finalizeConcreteInterfaceImplementations processedSchema;

  validateInterfaceImplementations schemaState;

  (* Remove any interface that isn't actually implemented by any type or
     interface. Otherwise the codegen we do for interfaces will error out on a
     bunch of impossible states. *)

  (* TODO: Could instead tag this as "not implemented by anyone" if we want to
     provide a nice hover message to make the user understand why it's not
     included in codegen.
  *)
  schemaState.interfaces
  |> iterHashtblAlphabetically (fun id (intf : gqlInterface) ->
      if Hashtbl.mem processedSchema.interfaceImplementedBy intf.id = false then
        Hashtbl.remove schemaState.interfaces id);

  GenerateSchemaValidation.validateSchema schemaState;
  processedSchema

let isPrintableArg (arg : gqlArg) =
  match arg.typ with
  | InjectContext | InjectInterfaceTypename _ | InjectInfo -> false
  | _ -> true

(** Some arguments aren't intended to be printed in the `args` list, like
    `InjectContext` which controls injecting context into the resolver. *)
let onlyPrintableArgs (args : gqlArg list) = args |> List.filter isPrintableArg

let isFileContentsTheSame filePath s =
  try
    let ic = open_in filePath in
    let fileLength = in_channel_length ic in
    let len = String.length s in
    if fileLength <> len then (
      close_in ic;
      false)
    else
      let contents = really_input_string ic fileLength in
      close_in ic;
      contents = s
  with Sys_error _ -> false

let gqlRegexp = Str.regexp_string "@gql."

let hasGqlAttribute str =
  try
    let _ = Str.search_forward gqlRegexp str 0 in
    true
  with Not_found -> false

let rec readLinesUntilValue fileChannel =
  try
    let line = input_line fileChannel in
    if hasGqlAttribute line then (
      close_in fileChannel;
      true)
    else readLinesUntilValue fileChannel
  with End_of_file ->
    close_in fileChannel;
    false

let fileHasGqlAttribute filePath =
  let fileChannel = open_in filePath in
  readLinesUntilValue fileChannel

let writeIfHasChanges path contents =
  if isFileContentsTheSame path contents then ()
  else
    try
      let oc = open_out path in

      output_string oc contents;
      close_out oc
    with Sys_error _ ->
      Printf.printf
        "Something went wrong trying to write to \"%s\". Make sure the \
         directory actually exists."
        path;
      exit 1

type persistedSchemaState = {
  version: int;
  schemaName: string;
  schemaState: schemaState;
  processedSchema: processedSchema;
}

type persistedLegacySchemaState = {
  version: int;
  schemaState: schemaState;
  processedSchema: processedSchema;
}

let stateFileMagic = "RESGRAPH_STATE\000"
let stateFileVersion = 3

let validStateName schemaName =
  Str.string_match (Str.regexp "^[A-Za-z0-9_-]+$") schemaName 0

let getStateFilePath ?schemaName (package : SharedTypes.package) =
  match schemaName with
  | None -> package.rootPath ^ "/lib/.resgraphState.marshal"
  | Some schemaName when validStateName schemaName ->
    package.rootPath ^ "/lib/resgraph/" ^ schemaName ^ ".state.marshal"
  | Some _ -> invalid_arg "Invalid ResGraph schema state name"

let stateFileExists ?schemaName (package : SharedTypes.package) =
  Files.exists (getStateFilePath ?schemaName package)

let ensureStateDirectory (package : SharedTypes.package) =
  let directory = package.rootPath ^ "/lib/resgraph" in
  if not (Files.exists directory) then Unix.mkdir directory 0o755

let writeStateFile ?schemaName ~package ~schemaState ~processedSchema () =
  (match schemaName with
  | None -> ()
  | Some _ -> ensureStateDirectory package);
  let ch = open_out_bin (getStateFilePath ?schemaName package) in
  output_string ch stateFileMagic;
  (match schemaName with
  | None ->
    Marshal.to_channel ch
      {version = stateFileVersion; schemaState; processedSchema}
      [Compat_32]
  | Some schemaName ->
    Marshal.to_channel ch
      {version = stateFileVersion; schemaName; schemaState; processedSchema}
      [Compat_32]);
  close_out ch

let readStateFile ?schemaName ~package () =
  let ch = open_in_bin (getStateFilePath ?schemaName package) in
  Fun.protect
    (fun () ->
      let magic = really_input_string ch (String.length stateFileMagic) in
      if magic <> stateFileMagic then
        failwith "Incompatible ResGraph schema state file";
      match schemaName with
      | None ->
        let persisted : persistedLegacySchemaState = Marshal.from_channel ch in
        if persisted.version <> stateFileVersion then
          failwith "Incompatible ResGraph schema state file"
        else (persisted.schemaState, persisted.processedSchema)
      | Some expectedSchemaName ->
        let persisted : persistedSchemaState = Marshal.from_channel ch in
        if
          persisted.version <> stateFileVersion
          || persisted.schemaName <> expectedSchemaName
        then failwith "Incompatible ResGraph schema state file"
        else (persisted.schemaState, persisted.processedSchema))
    ~finally:(fun () -> close_in_noerr ch)

type scalarValidationResult = DoesNotNeedParsing | NeedsParsing
let rec validateCustomScalar ~env ~package (typ : Types.type_expr) =
  match typ.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    validateCustomScalar ~env ~package t1
  | Tconstr (Path.Pident {name = "option" | "array"}, [payloadTypeExpr], _) ->
    validateCustomScalar ~env ~package payloadTypeExpr
  | Tconstr (Path.Pident {name = "bool" | "string" | "int" | "float"}, [], _) ->
    DoesNotNeedParsing
  | Tconstr (path, typeArgs, _) -> (
    match pathIdentToList path with
    | ["JSON"; "t"]
    | ["Js"; "Json"; "t"]
    | ["Js"; "Nullable"; "t"]
    | ["Nullable"; "t"]
    | ["Js"; "Null"; "t"]
    | ["Null"; "t"] ->
      DoesNotNeedParsing
    | _ -> (
      match References.digConstructor ~env ~package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = typeParams}};
            } ) ->
        let t1 = t1 |> TypeUtils.instantiateType ~typeParams ~typeArgs in
        validateCustomScalar ~env ~package t1
      | _ -> NeedsParsing))
  | _ -> NeedsParsing

let emptyLoc =
  {
    Location.loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos;
    loc_ghost = true;
  }

let lastModuleInPath modulePath =
  let rec loop modulePath current =
    match modulePath with
    | SharedTypes.ModulePath.File _ -> current
    | IncludedModule (_, inner) -> loop inner current
    | ExportedModule {name; modulePath = inner} -> loop inner name
    | NotVisible -> current
  in
  loop modulePath ""

let makeSnippets ~path ~schemaName =
  let baseSnippets =
    [
      ( "gql.type snippet - simple connection",
        "Boilerplate for creating a new simple GraphQL connection for \
         pagination.",
        {|@gql.type
/** An edge in a connection. */
type ${1:entity}Edge = {
  @gql.field
  cursor: string,
  @gql.field
  node: option<${1:entity}>,
}

/** A connection to a list of items. */
@gql.type
type ${1:entity}Connection = {
  @gql.field
  pageInfo: ResGraph.Connections.pageInfo,
  @gql.field
  edges: option<array<option<${1:entity}Edge>>>,
}|}
      );
      ( "gql.type snippet - full connection",
        "Boilerplate for creating a new GraphQL connection for pagination.",
        {|@gql.type
/** An edge in a connection. */
type ${1:entity}Edge = {
  /** A cursor for use in pagination. */
  @gql.field
  cursor: string,
  /** The item at the end of the edge. */
  @gql.field
  node: option<${1:entity}>
}

/** A connection to a list of items. */
@gql.type
type ${1:entity}Connection = {
  /** Information to aid in pagination. */
  @gql.field
  pageInfo: ResGraph.Connections.pageInfo,
  /** A list of edges. */
  @gql.field
  edges: option<array<option<${1:entity}Edge>>>
}|}
      );
      ( "gql.type snippet - field function on type",
        "Boilerplate for adding a new field to a type via a function.",
        {|gql.field
let ${1:fieldName} = async (${2:entity}: ${2:entity}) => {
  ${0:Some(entity.prop)}
}|}
      );
    ]
  in
  let extendedSnippets =
    match Packages.getPackage ~uri:(Uri.fromPath path) with
    | None -> []
    | Some package -> (
      let moduleName =
        path |> Filename.basename |> Filename.remove_extension
        |> capitalizeFirstChar
      in
      let schemaState =
        try Some (readStateFile ?schemaName ~package () |> fst)
        with Sys_error _ | End_of_file | Failure _ | Invalid_argument _ ->
          None
      in
      match schemaState with
      | None -> []
      | Some schemaState ->
        let contextType = String.concat "." schemaState.contextTypePath in
        let snippets = ref [] in
        (match schemaState.query with
        | Some {typeLocation = Some (Concrete typeLocation)} ->
          snippets :=
            !snippets
            @ [
                ( "gql.field snippet - query field",
                  "Boilerplate for adding a new field to the root query.",
                  Printf.sprintf
                    {|gql.field
let ${1:fieldName} = async (_: %s, ~ctx: %s) => {
  ${0:Some(entity.prop)}
}|}
                    (if typeLocation.fileName = moduleName then "query"
                     else typeLocationToAccessor typeLocation)
                    contextType );
              ]
        | _ -> ());
        (match schemaState.mutation with
        | Some {typeLocation = Some (Concrete typeLocation)} ->
          snippets :=
            !snippets
            @ [
                ( "gql.field snippet - full mutation",
                  "Boilerplate for adding a new mutation the mutation type.",
                  Printf.sprintf
                    {|gql.union
type ${1:mutationName}Result = Success({ok: bool}) | Error({reason: string})

@gql.field
let ${1:mutationName} = async (_: %s, ~ctx: %s) => {
  Success({ok: true})
}|}
                    (if typeLocation.fileName = moduleName then "mutation"
                     else typeLocationToAccessor typeLocation)
                    contextType );
              ]
        | _ -> ());
        !snippets)
  in
  baseSnippets @ extendedSnippets
