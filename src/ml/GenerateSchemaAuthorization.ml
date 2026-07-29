open GenerateSchemaTypes
open GenerateSchemaDiagnostics

let addAuthorizationDiagnostic schemaState
    (reference : authorizationFunctionReference) message =
  schemaState
  |> addDiagnostic
       ~diagnostic:{loc = reference.loc; fileUri = reference.fileUri; message}

let isUnitType ~env ~package typ =
  match (TypeUtils.expandTransparentAlias ~env ~package typ).desc with
  | Tconstr (path, [], _) -> Path.same path Predef.path_unit
  | _ -> false

let objectFieldNames ~env ~package typ =
  let rec fields acc typ =
    match (TypeUtils.expandTransparentAlias ~env ~package typ).desc with
    | Tfield (name, kind, _fieldType, rest) ->
      let acc =
        match Btype.field_kind_repr kind with
        | Fpresent -> name :: acc
        | Fabsent | Fvar _ -> acc
      in
      fields acc rest
    | Tnil | Tvar _ -> Some (List.rev acc)
    | _ -> None
  in
  match (TypeUtils.expandTransparentAlias ~env ~package typ).desc with
  | Tobject (row, _) -> fields [] row
  | Tvar _ -> Some []
  | _ -> None

let loadPolicyModule ~loader ~(package : SharedTypes.package) moduleName =
  let namespacedName =
    BuildSystem.namespacedName package.namespace moduleName
  in
  match loader ~moduleName:namespacedName with
  | Some file -> Some file
  | None when namespacedName <> moduleName -> loader ~moduleName
  | None -> None

let resolveFunction ~loader ~(package : SharedTypes.package)
    (reference : authorizationFunctionReference) =
  match reference.path with
  | [] -> None
  | moduleName :: nestedPath -> (
    match loadPolicyModule ~loader ~package moduleName with
    | None -> None
    | Some (file : SharedTypes.File.t) -> (
      match
        DirectResolve.resolvePath
          ~env:(SharedTypes.QueryEnv.fromFile file)
          ~path:nestedPath ~loader
      with
      | None -> None
      | Some (env, valueName) -> (
        match
          SharedTypes.Exported.find env.exported SharedTypes.Exported.Value
            valueName
        with
        | None -> None
        | Some stamp ->
          SharedTypes.Stamps.findValue env.file.stamps stamp
          |> Option.map
               (fun (declared : Types.type_expr SharedTypes.Declared.t) ->
                 (env, declared.item)))))

let graphqlTypeDisplayName = function
  | GraphQLObjectType {displayName} | GraphQLInterface {displayName} ->
    Some displayName
  | _ -> None

(* Generated policy calls use [Obj.magic] so interface policies can accept an
   interface source while concrete fields pass their implementing object.
   Validate the relationship here to keep that isolated cast safe and provide a
   source-located error. *)
let validateSource ~schemaState ~env ~package ~parentTypeName ~reference typ =
  match
    GenerateSchema.findGraphQLType typ ~debug:false ~env
      ~full:{SharedTypes.file = env.file; package}
      ~schemaState
  with
  | Some graphqlType
    when graphqlTypeDisplayName graphqlType = Some parentTypeName ->
    true
  | Some graphqlType ->
    addAuthorizationDiagnostic schemaState reference
      (Printf.sprintf
         "Authorization function `%s` has source type `%s`, but it is applied \
          to `%s`. Its first unlabelled argument must be the owning object or \
          interface type."
         (GenerateSchemaUtils.authorizationFunctionName reference)
         (match graphqlTypeDisplayName graphqlType with
         | Some displayName -> displayName
         | None -> "<non-object>")
         parentTypeName);
    false
  | None ->
    addAuthorizationDiagnostic schemaState reference
      (Printf.sprintf
         "Authorization function `%s` has an invalid source type. Its first \
          unlabelled argument must be `%s`."
         (GenerateSchemaUtils.authorizationFunctionName reference)
         parentTypeName);
    false

let validateArgsObject ~env ~package ~schemaState ~reference ~parentTypeName
    ~(field : gqlField) typ =
  match objectFieldNames ~env ~package typ with
  | None ->
    addAuthorizationDiagnostic schemaState reference
      (Printf.sprintf
         "Authorization function `%s` must declare `~args` as a ReScript \
          polymorphic object."
         (GenerateSchemaUtils.authorizationFunctionName reference));
    false
  | Some names ->
    let availableNames =
      field.args
      |> List.filter GenerateSchemaUtils.isPrintableArg
      |> List.map (fun (arg : gqlArg) -> arg.name)
    in
    let unavailable =
      names |> List.filter (fun name -> not (List.mem name availableNames))
    in
    if unavailable = [] then true
    else (
      addAuthorizationDiagnostic schemaState reference
        (Printf.sprintf
           "Authorization function `%s` requests unavailable field argument%s \
            %s on `%s.%s`."
           (GenerateSchemaUtils.authorizationFunctionName reference)
           (if List.length unavailable = 1 then "" else "s")
           (unavailable
           |> List.map (Printf.sprintf "`%s`")
           |> String.concat ", ")
           parentTypeName field.name);
      false)

let validateInjection ~schemaState ~env ~package ~reference label typ =
  match
    GenerateSchema.findGraphQLType typ ~debug:false ~env
      ~full:{SharedTypes.file = env.file; package}
      ~schemaState
  with
  | Some InjectContext when label = "ctx" -> Some AuthorizationContext
  | Some InjectInfo when label = "info" -> Some AuthorizationInfo
  | _ ->
    addAuthorizationDiagnostic schemaState reference
      (Printf.sprintf
         "Authorization function `%s` has invalid `~%s`. Only `~ctx: \
          ResGraphContext.context` and `~info: ResGraph.resolveInfo` are \
          supported injections."
         (GenerateSchemaUtils.authorizationFunctionName reference)
         label);
    None

let validateFunction ~loader ~(package : SharedTypes.package) ~schemaState
    ~parentTypeName ~sourceTypeName ~provenance ~(field : gqlField)
    (reference : authorizationFunctionReference) =
  match resolveFunction ~loader ~package reference with
  | None ->
    addAuthorizationDiagnostic schemaState reference
      (Printf.sprintf
         "Could not resolve authorization function `%s`. The path must name an \
          exported module-qualified function."
         (GenerateSchemaUtils.authorizationFunctionName reference));
    None
  | Some (env, typ) ->
    let args, returnType =
      GenerateSchema.extractFunctionType ~env ~package typ
    in
    let sourceValid =
      match args with
      | (Asttypes.Nolabel, sourceType) :: _ ->
        validateSource ~schemaState ~env ~package ~parentTypeName:sourceTypeName
          ~reference sourceType
      | _ ->
        addAuthorizationDiagnostic schemaState reference
          (Printf.sprintf
             "Authorization function `%s` must take the owning source as its \
              first unlabelled argument."
             (GenerateSchemaUtils.authorizationFunctionName reference));
        false
    in
    let remainingArgs =
      match args with
      | _ :: remainingArgs -> remainingArgs
      | [] -> []
    in
    let argsObject = ref None in
    let injections = ref [] in
    let labelsValid = ref true in
    remainingArgs
    |> List.iter (fun (label, typ) ->
        match label with
        | Asttypes.Labelled {txt = "args"} ->
          if Option.is_some !argsObject then (
            labelsValid := false;
            addAuthorizationDiagnostic schemaState reference
              (Printf.sprintf
                 "Authorization function `%s` must declare `~args` exactly \
                  once."
                 (GenerateSchemaUtils.authorizationFunctionName reference)))
          else argsObject := Some typ
        | Asttypes.Labelled {txt = ("ctx" | "info") as name} -> (
          match
            validateInjection ~schemaState ~env ~package ~reference name typ
          with
          | Some injection -> injections := !injections @ [injection]
          | None -> labelsValid := false)
        | Asttypes.Labelled {txt = name} | Asttypes.Optional {txt = name} ->
          labelsValid := false;
          addAuthorizationDiagnostic schemaState reference
            (Printf.sprintf
               "Authorization function `%s` has unsupported argument `%s`. \
                Supported labelled arguments are mandatory `~args` and \
                optional injections `~ctx` and `~info`."
               (GenerateSchemaUtils.authorizationFunctionName reference)
               name)
        | Asttypes.Nolabel ->
          labelsValid := false;
          addAuthorizationDiagnostic schemaState reference
            (Printf.sprintf
               "Authorization function `%s` can only have one unlabelled \
                argument: its source."
               (GenerateSchemaUtils.authorizationFunctionName reference)));
    let argsValid =
      match !argsObject with
      | None ->
        addAuthorizationDiagnostic schemaState reference
          (Printf.sprintf
             "Authorization function `%s` must declare the mandatory `~args` \
              polymorphic object, including for fields without arguments."
             (GenerateSchemaUtils.authorizationFunctionName reference));
        false
      | Some typ ->
        validateArgsObject ~env ~package ~schemaState ~reference ~parentTypeName
          ~field typ
    in
    let outcome =
      match
        GenerateSchema.extractAuthorizationOutcome ~env ~package returnType
      with
      | Some (allowedType, outcome) when isUnitType ~env ~package allowedType ->
        Some outcome
      | _ ->
        addAuthorizationDiagnostic schemaState reference
          (Printf.sprintf
             "Authorization function `%s` must return \
              `ResGraph.Authorization.outcome<unit, 'reason>` or a promise of \
              that outcome."
             (GenerateSchemaUtils.authorizationFunctionName reference));
        None
    in
    if sourceValid && argsValid && !labelsValid && Option.is_some outcome then
      let outcome = Option.get outcome in
      Some
        {
          reference;
          isAsync = outcome.isAsync;
          injections = !injections;
          provenance;
        }
    else None

let declaration schemaState coordinate =
  match Hashtbl.find_opt schemaState.authorizationDeclarations coordinate with
  | Some declaration -> declaration
  | None -> {functions = []; public = None}

type plannedReference = {
  reference: authorizationFunctionReference;
  sourceTypeName: string;
  provenance: authorizationProvenance;
}

let declaredReferences schemaState ~sourceTypeName ~provenance coordinate =
  (declaration schemaState coordinate).functions
  |> List.map (fun reference -> {reference; sourceTypeName; provenance})

let interfaceDeclarations schemaState (typ : gqlObjectType) fieldName =
  typ.interfaces
  |> List.fold_left
       (fun references interfaceId ->
         match Hashtbl.find_opt schemaState.interfaces interfaceId with
         | None -> references
         | Some intf ->
           let typeReferences =
             declaredReferences schemaState ~sourceTypeName:intf.displayName
               ~provenance:(InterfaceTypePolicy intf.displayName)
               intf.displayName
           in
           let fieldReferences =
             declaredReferences schemaState ~sourceTypeName:intf.displayName
               ~provenance:(InterfaceFieldPolicy intf.displayName)
               (GenerateSchemaUtils.authorizationCoordinate
                  ~parentTypeName:intf.displayName ~fieldName)
           in
           references @ typeReferences @ fieldReferences)
       []

let interfaceResolverOutcome schemaState (field : gqlField) =
  match field.inheritedFromInterface with
  | None -> None
  | Some parentTypeName ->
    Hashtbl.find_opt schemaState.resolverOutcomes
      (GenerateSchemaUtils.authorizationCoordinate ~parentTypeName
         ~fieldName:field.name)

let interfacePublic schemaState (typ : gqlObjectType) fieldName =
  typ.interfaces
  |> List.find_map (fun interfaceId ->
      match Hashtbl.find_opt schemaState.interfaces interfaceId with
      | None -> None
      | Some intf ->
        (declaration schemaState
           (GenerateSchemaUtils.authorizationCoordinate
              ~parentTypeName:intf.displayName ~fieldName))
          .public)

let buildFieldPlan ~loader ~package ~(schemaState : schemaState)
    ~(typ : gqlObjectType) ~(field : gqlField) =
  let coordinate =
    GenerateSchemaUtils.authorizationCoordinate ~parentTypeName:typ.displayName
      ~fieldName:field.name
  in
  let typeDeclaration = declaration schemaState typ.displayName in
  let fieldDeclaration = declaration schemaState coordinate in
  let references =
    (typeDeclaration.functions
    |> List.map (fun reference ->
        {
          reference;
          sourceTypeName = typ.displayName;
          provenance = ObjectTypePolicy typ.displayName;
        }))
    @ interfaceDeclarations schemaState typ field.name
    @ (fieldDeclaration.functions
      |> List.map (fun reference ->
          {
            reference;
            sourceTypeName = typ.displayName;
            provenance = FieldPolicy coordinate;
          }))
  in
  let functions =
    references
    |> List.filter_map (fun planned ->
        validateFunction ~loader ~package ~schemaState
          ~parentTypeName:typ.displayName ~sourceTypeName:planned.sourceTypeName
          ~provenance:planned.provenance ~field planned.reference)
  in
  let resolverOutcome =
    match Hashtbl.find_opt schemaState.resolverOutcomes coordinate with
    | Some outcome -> Some outcome
    | None -> interfaceResolverOutcome schemaState field
  in
  let synthetic = Hashtbl.mem schemaState.authorizationExemptions coordinate in
  let public =
    match fieldDeclaration.public with
    | Some public -> Some public
    | None -> interfacePublic schemaState typ field.name
  in
  let hasPolicies = references <> [] in
  (match public with
  | Some public when hasPolicies || Option.is_some resolverOutcome ->
    schemaState
    |> addDiagnostic
         ~diagnostic:
           {
             loc = public.loc;
             fileUri = public.fileUri;
             message =
               Printf.sprintf
                 "`%s` is declared public but also has authorization policy or \
                  resolver-outcome coverage. Public coverage cannot be \
                  combined with other authorization dispositions."
                 coordinate;
           }
  | _ -> ());
  Hashtbl.replace schemaState.authorizationPlans coordinate
    {functions; public; resolverOutcome; synthetic};
  match schemaState.authorizationConfig.mode with
  | AuthorizationOptional -> ()
  | AuthorizationRequired ->
    if typ.id = "subscription" then
      schemaState
      |> addDiagnostic
           ~diagnostic:
             {
               loc = field.loc;
               fileUri = field.fileUri;
               message =
                 "Required authorization coverage does not support \
                  subscriptions yet.";
             }
    else if synthetic then ()
    else if
      Option.is_none public && references = [] && Option.is_none resolverOutcome
    then
      schemaState
      |> addDiagnostic
           ~diagnostic:
             {
               loc = field.loc;
               fileUri = field.fileUri;
               message =
                 Printf.sprintf
                   "Field `%s` has no authorization disposition. Add \
                    `@gql.authorize(...)`, return \
                    `ResGraph.Authorization.outcome`, or declare \
                    `@gql.public({reason: \"...\"})`."
                   coordinate;
             }
    else if typ.id = "mutation" && Option.is_none public && references = [] then
      schemaState
      |> addDiagnostic
           ~diagnostic:
             {
               loc = field.loc;
               fileUri = field.fileUri;
               message =
                 Printf.sprintf
                   "Mutation field `%s` requires at least one pre-resolver \
                    `@gql.authorize(...)` policy. Resolver-outcome coverage \
                    alone runs after mutation side effects."
                   coordinate;
             }

let buildPlans ~loader ~package (schemaState : schemaState) =
  schemaState.types
  |> GenerateSchemaUtils.iterHashtblAlphabetically
       (fun _ (typ : gqlObjectType) ->
         typ.fields
         |> List.iter (fun field ->
             buildFieldPlan ~loader ~package ~schemaState ~typ ~field))

let jsonString value = Printf.sprintf "\"%s\"" (Json.escape value)

let relativeSourcePath ~(package : SharedTypes.package) fileUri =
  let path = Uri.toPath fileUri in
  let prefix = package.rootPath ^ Filename.dir_sep in
  if String.starts_with path ~prefix then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else path

let provenanceToString = function
  | ObjectTypePolicy name -> "objectType:" ^ name
  | InterfaceTypePolicy name -> "interfaceType:" ^ name
  | InterfaceFieldPolicy name -> "interfaceField:" ^ name
  | FieldPolicy coordinate -> "field:" ^ coordinate

let manifestPolicy ~package (fn : authorizationFunction) =
  Printf.sprintf
    "{\"path\":%s,\"provenance\":%s,\"file\":%s,\"location\":%s,\"async\":%s}"
    (jsonString (GenerateSchemaUtils.authorizationFunctionName fn.reference))
    (jsonString (provenanceToString fn.provenance))
    (jsonString (relativeSourcePath ~package fn.reference.fileUri))
    (jsonString (Loc.toString fn.reference.loc))
    (if fn.isAsync then "true" else "false")

let manifestField ~package coordinate (plan : effectiveAuthorizationPlan) =
  let disposition =
    match
      (plan.synthetic, plan.public, plan.functions, plan.resolverOutcome)
    with
    | true, _, _, _ -> "synthetic"
    | false, Some _, _, _ -> "public"
    | false, None, _ :: _, _ -> "policies"
    | false, None, [], Some _ -> "resolverOutcome"
    | false, None, [], None -> "uncovered"
  in
  let publicJson =
    match plan.public with
    | None -> "null"
    | Some public ->
      Printf.sprintf "{\"reason\":%s,\"file\":%s,\"location\":%s}"
        (jsonString public.reason)
        (jsonString (relativeSourcePath ~package public.fileUri))
        (jsonString (Loc.toString public.loc))
  in
  let resolverOutcomeJson =
    match plan.resolverOutcome with
    | None -> "null"
    | Some {isAsync} ->
      Printf.sprintf "{\"async\":%s}" (if isAsync then "true" else "false")
  in
  Printf.sprintf
    "{\"coordinate\":%s,\"disposition\":%s,\"mutation\":%s,\"policies\":[%s],\"public\":%s,\"resolverOutcome\":%s}"
    (jsonString coordinate) (jsonString disposition)
    (if String.starts_with coordinate ~prefix:"Mutation." then "true"
     else "false")
    (plan.functions |> List.map (manifestPolicy ~package) |> String.concat ",")
    publicJson resolverOutcomeJson

let rec ensureDirectory path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    ensureDirectory (Filename.dirname path);
    Unix.mkdir path 0o755)

let generatedManifestMarker = "\"generatedBy\": \"resgraph\""

let generatedManifest ~status ~fields =
  Printf.sprintf
    "{\n\
    \  \"generatedBy\": \"resgraph\",\n\
    \  \"version\": 1,\n\
    \  \"status\": \"%s\",\n\
    \  \"fields\": [\n\
    \    %s\n\
    \  ]\n\
     }\n"
    status fields

let isGeneratedManifest path =
  match Files.readFile path with
  | None -> false
  | Some contents -> (
    try
      ignore
        (Str.search_forward
           (Str.regexp_string generatedManifestMarker)
           contents 0);
      true
    with Not_found -> false)

let prepareManifest ~outputFolder ~writeSdlFile
    (authorizationConfig : authorizationConfig) =
  match authorizationConfig.manifestPath with
  | None -> ()
  | Some path ->
    let generatedOutputPaths =
      [
        outputFolder ^ "/ResGraphSchema.res";
        outputFolder ^ "/ResGraphSchema.resi";
      ]
      @ if writeSdlFile then [outputFolder ^ "/schema.graphql"] else []
    in
    if List.mem path generatedOutputPaths then
      failwith
        (Printf.sprintf
           "Authorization manifest path `%s` collides with a generated schema \
            artifact."
           path)
    else if Sys.file_exists path && not (isGeneratedManifest path) then
      failwith
        (Printf.sprintf
           "Refusing to overwrite authorization manifest path `%s` because it \
            contains a file not generated by ResGraph."
           path)
    else (
      ensureDirectory (Filename.dirname path);
      GenerateSchemaUtils.writeIfHasChanges path
        (generatedManifest ~status:"generationFailed" ~fields:""))

let writeManifest ~package (schemaState : schemaState) =
  match schemaState.authorizationConfig.manifestPath with
  | None -> ()
  | Some path ->
    ensureDirectory (Filename.dirname path);
    let fields =
      schemaState.authorizationPlans
      |> GenerateSchemaUtils.hashtblToListAlphabetically
      |> List.map (fun (coordinate, plan) ->
          manifestField ~package coordinate plan)
      |> String.concat ",\n    "
    in
    GenerateSchemaUtils.writeIfHasChanges path
      (generatedManifest ~status:"success" ~fields)
