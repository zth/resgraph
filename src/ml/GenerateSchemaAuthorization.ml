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

module BaselineEntry = struct
  type t = string * authorizationGapKind

  let compare = Stdlib.compare
end

module BaselineEntries = Set.Make (BaselineEntry)

let authorizationGapKindToString = function
  | UncoveredField -> "uncoveredField"
  | MutationPreResolverPolicy -> "mutationPreResolverPolicy"
  | UnsupportedSubscription -> "unsupportedSubscription"

let authorizationGapKindFromString = function
  | "uncoveredField" -> Some UncoveredField
  | "mutationPreResolverPolicy" -> Some MutationPreResolverPolicy
  | "unsupportedSubscription" -> Some UnsupportedSubscription
  | _ -> None

let invalidBaseline path message =
  failwith
    (Printf.sprintf "Invalid authorization baseline `%s`: %s" path message)

let loadBaseline path =
  let json =
    match Files.readFile path with
    | None ->
      failwith
        (Printf.sprintf
           "Authorization baseline `%s` does not exist. Run `resgraph \
            authorization baseline` to create it."
           path)
    | Some contents -> (
      match Json.parse contents with
      | Some json -> json
      | None -> invalidBaseline path "expected valid JSON.")
  in
  let stringProperty name = Option.bind (Json.get name json) Json.string in
  let numberProperty name = Option.bind (Json.get name json) Json.number in
  let gaps = Option.bind (Json.get "gaps" json) Json.array in
  if stringProperty "generatedBy" <> Some "resgraph" then
    invalidBaseline path "missing the ResGraph generated-file marker."
  else if stringProperty "kind" <> Some "authorizationBaseline" then
    invalidBaseline path "expected kind `authorizationBaseline`."
  else if numberProperty "version" <> Some 1. then
    invalidBaseline path "expected version 1."
  else
    match gaps with
    | None -> invalidBaseline path "expected a `gaps` array."
    | Some gaps ->
      gaps
      |> List.fold_left
           (fun entries gap ->
             let coordinate =
               Option.bind (Json.get "coordinate" gap) Json.string
             in
             let kind =
               Option.bind (Json.get "kind" gap) Json.string |> fun kind ->
               Option.bind kind authorizationGapKindFromString
             in
             match (coordinate, kind) with
             | Some coordinate, Some kind ->
               let entry = (coordinate, kind) in
               if BaselineEntries.mem entry entries then
                 invalidBaseline path
                   (Printf.sprintf "duplicate gap `%s` (`%s`)." coordinate
                      (authorizationGapKindToString kind))
               else BaselineEntries.add entry entries
             | _ ->
               invalidBaseline path
                 "every gap must have a string `coordinate` and a supported \
                  `kind`.")
           BaselineEntries.empty

let gapForField ~(typ : gqlObjectType) ~(field : gqlField) ~synthetic ~public
    ~references ~resolverOutcome =
  let coordinate =
    GenerateSchemaUtils.authorizationCoordinate ~parentTypeName:typ.displayName
      ~fieldName:field.name
  in
  if typ.id = "subscription" then Some (coordinate, UnsupportedSubscription)
  else if synthetic then None
  else if
    Option.is_none public && references = [] && Option.is_none resolverOutcome
  then Some (coordinate, UncoveredField)
  else if typ.id = "mutation" && Option.is_none public && references = [] then
    Some (coordinate, MutationPreResolverPolicy)
  else None

let addGapDiagnostic schemaState ~(field : gqlField) (coordinate, kind) =
  let message =
    match kind with
    | UnsupportedSubscription ->
      Printf.sprintf
        "Required authorization coverage does not support subscription field \
         `%s` yet."
        coordinate
    | UncoveredField ->
      Printf.sprintf
        "Field `%s` has no authorization disposition. Add \
         `@gql.authorize(...)`, return `ResGraph.Authorization.outcome`, or \
         declare `@gql.public({reason: \"...\"})`."
        coordinate
    | MutationPreResolverPolicy ->
      Printf.sprintf
        "Mutation field `%s` requires at least one pre-resolver \
         `@gql.authorize(...)` policy. Resolver-outcome coverage alone runs \
         after mutation side effects."
        coordinate
  in
  schemaState
  |> addDiagnostic
       ~diagnostic:{loc = field.loc; fileUri = field.fileUri; message}

let buildFieldPlan ~loader ~package ~(schemaState : schemaState)
    ~baselineEntries ~skipGapDiagnostics ~(typ : gqlObjectType)
    ~(field : gqlField) =
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
  let gap =
    gapForField ~typ ~field ~synthetic ~public ~references ~resolverOutcome
  in
  let baselineGap =
    match (schemaState.authorizationConfig.mode, gap, baselineEntries) with
    | AuthorizationBaseline, Some (_, kind), _ -> Some kind
    | _, Some entry, Some entries when BaselineEntries.mem entry entries ->
      let _, kind = entry in
      Some kind
    | _ -> None
  in
  Hashtbl.replace schemaState.authorizationPlans coordinate
    {functions; public; resolverOutcome; synthetic; baselineGap};
  (match gap with
  | Some entry ->
    schemaState.authorizationGaps <- entry :: schemaState.authorizationGaps
  | None -> ());
  match schemaState.authorizationConfig.mode with
  | AuthorizationOptional | AuthorizationBaseline -> ()
  | AuthorizationRequired -> (
    if not skipGapDiagnostics then
      match (gap, baselineGap) with
      | Some entry, None -> addGapDiagnostic schemaState ~field entry
      | _ -> ())

let addStaleBaselineDiagnostics schemaState ~path ~baselineEntries =
  let actualEntries = BaselineEntries.of_list schemaState.authorizationGaps in
  BaselineEntries.diff baselineEntries actualEntries
  |> BaselineEntries.iter (fun (coordinate, kind) ->
      schemaState
      |> addDiagnostic
           ~diagnostic:
             {
               loc = Location.none;
               fileUri = Uri.fromPath path;
               message =
                 Printf.sprintf
                   "Authorization baseline entry `%s` (`%s`) is stale. Remove \
                    it from the baseline."
                   coordinate
                   (authorizationGapKindToString kind);
             })

let buildPlans ~loader ~package (schemaState : schemaState) =
  let baselineEntries, baselineLoadFailed =
    match
      ( schemaState.authorizationConfig.mode,
        schemaState.authorizationConfig.baselinePath )
    with
    | AuthorizationRequired, Some path -> (
      try (Some (loadBaseline path), false)
      with Failure message ->
        schemaState
        |> addDiagnostic
             ~diagnostic:
               {loc = Location.none; fileUri = Uri.fromPath path; message};
        (None, true))
    | _ -> (None, false)
  in
  schemaState.types
  |> GenerateSchemaUtils.iterHashtblAlphabetically
       (fun _ (typ : gqlObjectType) ->
         typ.fields
         |> List.iter (fun field ->
             buildFieldPlan ~loader ~package ~schemaState ~baselineEntries
               ~skipGapDiagnostics:baselineLoadFailed ~typ ~field));
  match (baselineEntries, schemaState.authorizationConfig.baselinePath) with
  | Some entries, Some path ->
    addStaleBaselineDiagnostics schemaState ~path ~baselineEntries:entries
  | _ -> ()

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
      ( plan.baselineGap,
        plan.synthetic,
        plan.public,
        plan.functions,
        plan.resolverOutcome )
    with
    | Some _, _, _, _, _ -> "baseline"
    | None, true, _, _, _ -> "synthetic"
    | None, false, Some _, _, _ -> "public"
    | None, false, None, _ :: _, _ -> "policies"
    | None, false, None, [], Some _ -> "resolverOutcome"
    | None, false, None, [], None -> "uncovered"
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
    match Json.parse contents with
    | None -> false
    | Some json ->
      let status = Option.bind (Json.get "status" json) Json.string in
      Option.bind (Json.get "generatedBy" json) Json.string = Some "resgraph"
      && Option.bind (Json.get "version" json) Json.number = Some 1.
      && (status = Some "generationFailed" || status = Some "success")
      && Option.is_some (Option.bind (Json.get "fields" json) Json.array))

let isGeneratedBaseline path =
  match Files.readFile path with
  | None -> false
  | Some contents -> (
    match Json.parse contents with
    | None -> false
    | Some json ->
      Option.bind (Json.get "generatedBy" json) Json.string = Some "resgraph"
      && Option.bind (Json.get "kind" json) Json.string
         = Some "authorizationBaseline"
      && Option.bind (Json.get "version" json) Json.number = Some 1.)

let isInterfaceArtifactFileName fileName =
  let fileName = String.lowercase_ascii fileName in
  (String.starts_with fileName ~prefix:"interface_"
  || Str.string_match (Str.regexp ".*__interface_.*\\.res$") fileName 0)
  && Filename.check_suffix fileName ".res"

let collidesWithInterfaceArtifact ~outputFolder path =
  Files.sameFile (Filename.dirname path) outputFolder
  && isInterfaceArtifactFileName (Filename.basename path)
  ||
    try
      Sys.readdir outputFolder
      |> Array.exists (fun fileName ->
          isInterfaceArtifactFileName fileName
          && Files.sameFile path (Filename.concat outputFolder fileName))
    with Sys_error _ -> false

let validateBaselineOutputPath ~outputFolder ~writeSdlFile
    ~additionalOutputPaths (authorizationConfig : authorizationConfig) =
  (match
     (authorizationConfig.baselinePath, authorizationConfig.manifestPath)
   with
  | Some baselinePath, Some manifestPath
    when Files.sameFile baselinePath manifestPath ->
    failwith
      (Printf.sprintf
         "Authorization baseline path `%s` collides with the authorization \
          manifest path."
         baselinePath)
  | _ -> ());
  match (authorizationConfig.mode, authorizationConfig.baselinePath) with
  | AuthorizationBaseline, None ->
    failwith
      "`authorization.baselinePath` must be configured before running \
       `resgraph authorization baseline`."
  | ((AuthorizationBaseline | AuthorizationRequired) as mode), Some path ->
    let generatedOutputPaths =
      [
        outputFolder ^ "/ResGraphSchema.res";
        outputFolder ^ "/ResGraphSchema.resi";
      ]
      @ (if writeSdlFile then [outputFolder ^ "/schema.graphql"] else [])
      @ additionalOutputPaths
      @
      match authorizationConfig.manifestPath with
      | Some manifestPath -> [manifestPath]
      | None -> []
    in
    let collidesWithInterfaceFile =
      collidesWithInterfaceArtifact ~outputFolder path
    in
    if
      List.exists
        (fun generatedPath -> Files.sameFile path generatedPath)
        generatedOutputPaths
      || collidesWithInterfaceFile
    then
      failwith
        (Printf.sprintf
           "Authorization baseline path `%s` collides with a generated schema \
            artifact."
           path)
    else if
      mode = AuthorizationBaseline
      && Sys.file_exists path
      && not (isGeneratedBaseline path)
    then
      failwith
        (Printf.sprintf
           "Refusing to overwrite authorization baseline path `%s` because it \
            contains a file not generated by ResGraph."
           path)
  | AuthorizationOptional, _ | AuthorizationRequired, None -> ()

let generatedBaseline gaps =
  let gaps =
    gaps
    |> List.sort_uniq Stdlib.compare
    |> List.map (fun (coordinate, kind) ->
        Printf.sprintf "{\"coordinate\":%s,\"kind\":%s}" (jsonString coordinate)
          (jsonString (authorizationGapKindToString kind)))
    |> String.concat ",\n    "
  in
  Printf.sprintf
    "{\n\
    \  \"generatedBy\": \"resgraph\",\n\
    \  \"kind\": \"authorizationBaseline\",\n\
    \  \"version\": 1,\n\
    \  \"gaps\": [\n\
    \    %s\n\
    \  ]\n\
     }\n"
    gaps

let writeBaseline (schemaState : schemaState) =
  match
    ( schemaState.authorizationConfig.mode,
      schemaState.authorizationConfig.baselinePath )
  with
  | AuthorizationBaseline, Some path ->
    ensureDirectory (Filename.dirname path);
    GenerateSchemaUtils.writeIfHasChanges path
      (generatedBaseline schemaState.authorizationGaps)
  | _ -> ()

let prepareManifest ~outputFolder ~writeSdlFile ~additionalOutputPaths
    (authorizationConfig : authorizationConfig) =
  match authorizationConfig.manifestPath with
  | None -> ()
  | Some path ->
    let generatedOutputPaths =
      [
        outputFolder ^ "/ResGraphSchema.res";
        outputFolder ^ "/ResGraphSchema.resi";
      ]
      @ (if writeSdlFile then [outputFolder ^ "/schema.graphql"] else [])
      @ additionalOutputPaths
    in
    let collidesWithInterfaceFile =
      collidesWithInterfaceArtifact ~outputFolder path
    in
    if
      List.exists
        (fun generatedPath -> Files.sameFile path generatedPath)
        generatedOutputPaths
      || collidesWithInterfaceFile
    then
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
