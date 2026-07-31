open GenerateSchemaTypes
open GenerateSchemaUtils

type context = CtxDefault | CtxInterface | CtxSubscription

let printLabelledArg name =
  if Res_token.is_keyword_txt name then Printf.sprintf "~\\\"%s\"" name
  else Printf.sprintf "~%s" name

let printAuthorizationArgs (field : gqlField) =
  let fields =
    field.args |> List.filter isPrintableArg
    |> List.sort (fun (a1 : gqlArg) a2 -> String.compare a1.name a2.name)
    |> List.map (fun (arg : gqlArg) ->
        Printf.sprintf "\"%s\": %s" arg.name
          (generateConverter (Printf.sprintf "args[\"%s\"]" arg.name) arg.typ))
  in
  match fields with
  | [] -> "%raw(`{}`)"
  | fields -> Printf.sprintf "{%s}" (String.concat ", " fields)

let printForbidden ~schemaState =
  match schemaState.authorizationConfig.onForbidden with
  | None -> "ResGraph.Authorization.raiseForbidden(reason)"
  | Some handler ->
    Printf.sprintf
      "ResGraph.Authorization.raiseError(%s(reason, ~ctx=ctx, ~info=info))"
      handler

let printPolicyCall (fn : authorizationFunction) =
  Printf.sprintf "%s(Obj.magic(src), ~args=authorizationArgs%s)"
    (authorizationFunctionName fn.reference)
    (fn.injections
    |> List.map (function
      | AuthorizationContext -> ", ~ctx=ctx"
      | AuthorizationInfo -> ", ~info=info")
    |> String.concat "")

let printResolverForField ~parentTypeName ~(schemaState : schemaState)
    (field : gqlField) =
  let coordinate =
    authorizationCoordinate ~parentTypeName ~fieldName:field.name
  in
  let plan =
    match Hashtbl.find_opt schemaState.authorizationPlans coordinate with
    | Some plan -> plan
    | None ->
      {
        functions = [];
        public = None;
        resolverOutcome =
          Hashtbl.find_opt schemaState.resolverOutcomes coordinate;
        synthetic = Hashtbl.mem schemaState.authorizationExemptions coordinate;
        baselineGap = None;
      }
  in
  let usesAuthorizationArgs = plan.functions <> [] in
  let resolverCall =
    match field.resolverStyle with
    | Property name -> Printf.sprintf "src[\"%s\"]" name
    | Resolver {moduleName; fnName; pathToFn} ->
      let ctxArgName = findContextArgName field.args in
      let hasCtxArg = Option.is_some ctxArgName in
      let infoArgName = findInfoArgName field.args in
      let hasInfoArg = Option.is_some infoArgName in
      let intfTypeArgName = findInterfaceTypeArgName field.args in
      let hasIntTypeArg = Option.is_some intfTypeArgName in
      Printf.sprintf "%s(src%s)"
        ([moduleName] @ pathToFn @ [fnName] |> String.concat ".")
        (if field.args = [] then ""
         else
           ", "
           ^ (field.args
             |> List.sort (fun (a1 : gqlArg) a2 ->
                 String.compare a1.name a2.name)
             |> List.filter_map (fun (arg : gqlArg) ->
                 if hasInfoArg && Some arg.name = infoArgName then
                   Some
                     (Printf.sprintf "%s=info"
                        (printLabelledArg (Option.get infoArgName)))
                 else if hasCtxArg && Some arg.name = ctxArgName then
                   Some
                     (Printf.sprintf "%s=ctx"
                        (printLabelledArg (Option.get ctxArgName)))
                 else if hasIntTypeArg && Some arg.name = intfTypeArgName then
                   field.onType
                   |> Option.map (fun name ->
                       Printf.sprintf "%s=%s"
                         (printLabelledArg (Option.get intfTypeArgName))
                         name)
                 else
                   let argsText =
                     if usesAuthorizationArgs then
                       Printf.sprintf "authorizationArgs[\"%s\"]" arg.name
                     else
                       generateConverter
                         (Printf.sprintf "args[\"%s\"]" arg.name)
                         arg.typ
                   in
                   Some
                     (Printf.sprintf "%s=%s"
                        (printLabelledArg arg.name)
                        (if arg.isOptionLabelled then
                           Printf.sprintf "?(%s)" argsText
                         else argsText)))
             |> String.concat ", "))
  in
  let resolverBody =
    match plan.resolverOutcome with
    | None -> resolverCall
    | Some {isAsync} ->
      Printf.sprintf
        "switch %s%s { | ResGraph.Authorization.Allowed(value) => value | \
         ResGraph.Authorization.Forbidden(reason) => %s }"
        (if isAsync then "await " else "")
        resolverCall
        (printForbidden ~schemaState)
  in
  let authorizedBody =
    List.fold_right
      (fun (fn : authorizationFunction) next ->
        Printf.sprintf
          "switch %s%s { | ResGraph.Authorization.Allowed() => %s | \
           ResGraph.Authorization.Forbidden(reason) => %s }"
          (if fn.isAsync then "await " else "")
          (printPolicyCall fn) next
          (printForbidden ~schemaState))
      plan.functions resolverBody
  in
  let isAsync =
    List.exists (fun (fn : authorizationFunction) -> fn.isAsync) plan.functions
    ||
    match plan.resolverOutcome with
    | Some {isAsync = true} -> true
    | _ -> false
  in
  let resolverArguments =
    match (field.resolverStyle, plan.functions, plan.resolverOutcome) with
    | Property _, [], None -> "(src, _args, _ctx, _info)"
    | _ -> "(src, args, ctx, info)"
  in
  Printf.sprintf "%s%s => {let src = typeUnwrapper(src); %s%s}"
    (if isAsync then "async " else "")
    resolverArguments
    (if usesAuthorizationArgs then
       Printf.sprintf "let authorizationArgs = %s; "
         (printAuthorizationArgs field)
     else "")
    authorizedBody

let rec printGraphQLType ?(nullable = false) (returnType : graphqlType) =
  let nullablePostfix = if nullable then "" else "->nonNull" in
  match returnType with
  | List inner ->
    Printf.sprintf "GraphQLListType.make(%s)->GraphQLListType.toGraphQLType%s"
      (printGraphQLType inner) nullablePostfix
  | RescriptNullable inner | Nullable inner ->
    printGraphQLType ~nullable:true inner
  | EmptyPayload -> printGraphQLType ~nullable:true (Scalar Boolean)
  | Scalar scalar ->
    let scalarStr =
      match scalar with
      | String -> "string"
      | Int -> "int"
      | Float -> "float"
      | ID -> "id"
      | Boolean -> "boolean"
    in
    Printf.sprintf "Scalars.%s->Scalars.toGraphQLType%s" scalarStr
      nullablePostfix
  | GraphQLObjectType {displayName} ->
    Printf.sprintf "get_%s()->GraphQLObjectType.toGraphQLType%s" displayName
      nullablePostfix
  | GraphQLScalar {displayName} ->
    Printf.sprintf "scalar_%s->GraphQLScalar.toGraphQLType%s" displayName
      nullablePostfix
  | GraphQLInterface {displayName} ->
    Printf.sprintf "get_%s()->GraphQLInterfaceType.toGraphQLType%s" displayName
      nullablePostfix
  | InjectInterfaceTypename {interfaceId = intfId} ->
    (* TODO: Kill in refactor. This is weird and shouldn't be needed. *)
    Printf.sprintf "get_%s()->GraphQLInterfaceType.toGraphQLType%s"
      (capitalizeFirstChar intfId)
      nullablePostfix
  | GraphQLInputUnion {displayName} | GraphQLInputObject {displayName} ->
    Printf.sprintf "get_%s()->GraphQLInputObjectType.toGraphQLType%s"
      displayName nullablePostfix
  | GraphQLEnum {displayName} ->
    Printf.sprintf "enum_%s->GraphQLEnumType.toGraphQLType%s" displayName
      nullablePostfix
  | GraphQLUnion {displayName} ->
    Printf.sprintf "get_%s()->GraphQLUnionType.toGraphQLType%s" displayName
      nullablePostfix
  | InjectContext | InjectInfo -> "Obj.magic()"

let floatLiteral value =
  if
    String.contains value '.' || String.contains value 'e'
    || String.contains value 'E'
  then value
  else value ^ "."

let rec printConstValue = function
  | ConstNull -> "GraphQLLiteralValue.Null"
  | ConstInt value | ConstFloat value ->
    Printf.sprintf "GraphQLLiteralValue.Number(%s)" (floatLiteral value)
  | ConstString value | ConstEnum value ->
    Printf.sprintf "GraphQLLiteralValue.String(\"%s\")" (Json.escape value)
  | ConstBoolean true -> "GraphQLLiteralValue.True"
  | ConstBoolean false -> "GraphQLLiteralValue.False"
  | ConstList values ->
    Printf.sprintf "GraphQLLiteralValue.Array([%s])"
      (values |> List.map printConstValue |> String.concat ", ")
  | ConstObject fields ->
    Printf.sprintf "GraphQLLiteralValue.Object(dict{%s})"
      (fields
      |> List.map (fun (name, value) ->
          Printf.sprintf "\"%s\": %s" name (printConstValue value))
      |> String.concat ", ")

let printDirectiveArguments arguments =
  Printf.sprintf "dict{%s}"
    (arguments
    |> List.map (fun (name, value) ->
        Printf.sprintf "\"%s\": %s" name (printConstValue value))
    |> String.concat ", ")

let groupDirectiveApplications applications =
  applications
  |> List.fold_left
       (fun groups (application : gqlDirectiveApplication) ->
         let rec add = function
           | [] -> [(application.name, [application.arguments])]
           | (name, arguments) :: rest when name = application.name ->
             (name, arguments @ [application.arguments]) :: rest
           | group :: rest -> group :: add rest
         in
         add groups)
       []

let printDirectiveExtensions schemaState target =
  let applications =
    GenerateSchemaUtils.directivesForTarget schemaState target
  in
  if applications = [] then None
  else
    let fields = ref [] in
    (if applications <> [] then
       let directives =
         applications |> groupDirectiveApplications
         |> List.map (fun (name, argumentSets) ->
             Printf.sprintf "\"%s\": [%s]" name
               (argumentSets
               |> List.map printDirectiveArguments
               |> String.concat ", "))
         |> String.concat ", "
       in
       let ordered =
         applications
         |> List.map (fun (application : gqlDirectiveApplication) ->
             Printf.sprintf "{name: \"%s\", args: %s}" application.name
               (printDirectiveArguments application.arguments))
         |> String.concat ", "
       in
       fields :=
         !fields
         @ [
             Printf.sprintf "directives: dict{%s}" directives;
             Printf.sprintf "resgraph: {appliedDirectives: [%s]}" ordered;
           ]);
    Some (Printf.sprintf "{%s}" (String.concat ", " !fields))

let displayNameFromImplementedBy
    (interfaceImplementedBy : interfaceImplementedBy) =
  match interfaceImplementedBy with
  | ObjectType {displayName} | Interface {displayName} -> displayName

let idFromImplementedBy (interfaceImplementedBy : interfaceImplementedBy) =
  match interfaceImplementedBy with
  | ObjectType {id} | Interface {id} -> id

let typeLocationFromImplementedBy
    (interfaceImplementedBy : interfaceImplementedBy) =
  match interfaceImplementedBy with
  | ObjectType {typeLocation = Some (Concrete typeLocation)}
  | Interface {typeLocation} ->
    typeLocationToAccessor typeLocation
  | ObjectType _ -> raise (Failure "Error code: TLFIB_MTL")

let sortImplementedBy (a1 : interfaceImplementedBy) a2 =
  String.compare
    (displayNameFromImplementedBy a1)
    (displayNameFromImplementedBy a2)

let printInterfaceResolverReturnType
    (gqlInterfaceIdentifier : gqlInterfaceIdentifier)
    ~(implementedBy : interfaceImplementedBy list) =
  Printf.sprintf "@gql.interfaceResolver(\"%s\")\ntype t = %s"
    gqlInterfaceIdentifier.id
    (implementedBy
    |> List.sort sortImplementedBy
    |> List.map (fun (i : interfaceImplementedBy) ->
        Printf.sprintf "%s(%s)"
          (displayNameFromImplementedBy i)
          (typeLocationFromImplementedBy i))
    |> String.concat " | ")

let printInterfaceImplementedByType
    ~(implementedBy : interfaceImplementedBy list) =
  if List.length implementedBy = 0 then ""
  else
    Printf.sprintf "  type t = %s"
      (implementedBy
      |> List.sort sortImplementedBy
      |> List.map (fun (i : interfaceImplementedBy) ->
          displayNameFromImplementedBy i)
      |> String.concat " | ")

let printNodeInterfaceAssets (implementedBy : interfaceImplementedBy list) =
  if List.length implementedBy = 0 then ""
  else
    Printf.sprintf "type typeMap<'a> = {\n%s\n}\n\n"
      (implementedBy
      |> List.sort sortImplementedBy
      |> List.map (fun (i : interfaceImplementedBy) ->
          Printf.sprintf "  @as(\"%s\") %s: 'a,"
            (displayNameFromImplementedBy i)
            (idFromImplementedBy i))
      |> String.concat "\n")
    ^ Printf.sprintf
        {|module TypeMap: {
  type t<'value>
  let make: (typeMap<'value>, ~valueToString: 'value => string) => t<'value>

  /** Takes a (stringified) value and returns what type it represents, if any. */
  let getTypeByStringifiedValue: (t<'value>, string) => option<ImplementedBy.t>

  /** Takes a type and returns what value it represents, as string. */
  let getStringifiedValueByType: (t<'value>, ImplementedBy.t) => string
} = {
  external unsafe_toDict: typeMap<'value> => dict<'value> = "%%identity"
  external unsafe_toType: string => ImplementedBy.t = "%%identity"
  type t<'value> = {
    typeToValue: dict<'value>,
    valueToTypeAsString: dict<string>,
    valueToString: 'value => string,
  }
  let make = (typeMap, ~valueToString) => {
    typeToValue: typeMap->unsafe_toDict,
    valueToTypeAsString: typeMap
    ->unsafe_toDict
    ->Dict.toArray
    ->Array.map(((key, value)) => (valueToString(value), key))
    ->Dict.fromArray,
    valueToString,
  }

  let getStringifiedValueByType = (t, typ) =>
    t.typeToValue
    ->Dict.get(typ->ImplementedBy.toString)
    ->Option.getOrThrow
    ->t.valueToString
  let getTypeByStringifiedValue = (t, str) =>
    t.valueToTypeAsString->Dict.get(str)->Option.map(unsafe_toType)
}|}

let printInterfaceTypenameDecoder ~(implementedBy : interfaceImplementedBy list)
    =
  if List.length implementedBy = 0 then ""
  else
    Printf.sprintf
      "let decode = (str: string) => switch str {\n%s\n  | _ => None\n}"
      (implementedBy
      |> List.sort sortImplementedBy
      |> List.map (fun (item : interfaceImplementedBy) ->
          let displayName = displayNameFromImplementedBy item in
          Printf.sprintf "  | \"%s\" => Some(%s)" displayName displayName)
      |> String.concat "\n")

let printInterfaceTypenameToString
    ~(implementedBy : interfaceImplementedBy list) =
  if List.length implementedBy = 0 then ""
  else Printf.sprintf "external toString: t => string = \"%%identity\""

let printArg ~schemaState ~parentTypeName ~fieldName (arg : gqlArg) =
  let extensions =
    printDirectiveExtensions schemaState
      (DirectiveArgumentDefinition
         {parentTypeName; fieldName; argumentName = arg.name})
  in
  match
    (arg.defaultValue, arg.description, arg.deprecationReason, extensions)
  with
  | None, None, None, None ->
    Printf.sprintf "({typ: %s}: arg)" (printGraphQLType arg.typ)
  | _ ->
    let writer = CodeWriter.create 192 in
    CodeWriter.line writer "({";
    CodeWriter.indented writer (fun () ->
        CodeWriter.line writer
          (Printf.sprintf "typ: %s," (printGraphQLType arg.typ));
        (match arg.defaultValue with
        | None -> ()
        | Some value ->
          CodeWriter.line writer
            (Printf.sprintf "defaultValue: %s," (printConstValue value)));
        (match arg.description with
        | None -> ()
        | Some description ->
          CodeWriter.line writer (Printf.sprintf "description: %S," description));
        (match arg.deprecationReason with
        | None -> ()
        | Some reason ->
          CodeWriter.line writer
            (Printf.sprintf "deprecationReason: %S," reason));
        match extensions with
        | None -> ()
        | Some extensions ->
          CodeWriter.line writer (Printf.sprintf "extensions: %s" extensions));
    CodeWriter.add writer "}: arg)";
    CodeWriter.contents writer

let printArgs ~schemaState ~parentTypeName ~fieldName (args : gqlArg list) =
  let args =
    args
    |> List.sort (fun (a1 : gqlArg) a2 -> String.compare a1.name a2.name)
    |> List.filter isPrintableArg
  in
  let writer = CodeWriter.create 256 in
  let lastArgIndex = List.length args - 1 in
  CodeWriter.line writer "dict{";
  CodeWriter.indented writer (fun () ->
      args
      |> List.iteri (fun index (arg : gqlArg) ->
          CodeWriter.line writer
            (Printf.sprintf "\"%s\": %s%s" arg.name
               (printArg ~schemaState ~parentTypeName ~fieldName arg)
               (if index = lastArgIndex then "" else ","))));
  CodeWriter.add writer "}->makeArgsDict";
  CodeWriter.contents writer

let printDirectiveArgument schemaState directiveName
    (argument : gqlDirectiveArgument) =
  let writer = CodeWriter.create 256 in
  CodeWriter.line writer "({";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer
        (Printf.sprintf "typ: %s," (printGraphQLType argument.typ));
      (match argument.defaultValue with
      | None -> ()
      | Some value ->
        CodeWriter.line writer
          (Printf.sprintf "defaultValue: %s," (printConstValue value)));
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (descriptionAsString argument.description));
      CodeWriter.line writer
        (Printf.sprintf "deprecationReason: %s,"
           (undefinedOrValueAsString argument.deprecationReason));
      match
        printDirectiveExtensions schemaState
          (DirectiveDirectiveArgumentDefinition
             {directiveName; argumentName = argument.name})
      with
      | None -> ()
      | Some extensions ->
        CodeWriter.line writer (Printf.sprintf "extensions: %s" extensions));
  CodeWriter.add writer "}: arg)";
  CodeWriter.contents writer

let printDirectiveDefinition schemaState (definition : gqlDirectiveDefinition) =
  let writer = CodeWriter.create 512 in
  CodeWriter.line writer "{";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer (Printf.sprintf "name: \"%s\"," definition.name);
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (descriptionAsString definition.description));
      CodeWriter.line writer
        (Printf.sprintf "locations: [%s],"
           (definition.locations
           |> List.map (fun location ->
               Printf.sprintf "\"%s\""
                 (GenerateSchemaDirectiveUtils.locationToString location))
           |> String.concat ", "));
      if definition.arguments <> [] then (
        CodeWriter.line writer "args: dict{";
        CodeWriter.indented writer (fun () ->
            definition.arguments
            |> List.iteri (fun index (argument : gqlDirectiveArgument) ->
                CodeWriter.add writer (Printf.sprintf "\"%s\": " argument.name);
                CodeWriter.add writer
                  (printDirectiveArgument schemaState definition.name argument);
                CodeWriter.line writer
                  (if index = List.length definition.arguments - 1 then ""
                   else ",")));
        CodeWriter.line writer "}->makeArgsDict,");
      CodeWriter.line writer
        (Printf.sprintf "isRepeatable: %s"
           (if definition.repeatable then "true" else "false")));
  CodeWriter.add writer "}";
  CodeWriter.contents writer

let printField ?(context = CtxDefault) ~parentTypeName ~schemaState
    (field : gqlField) =
  let printableArgs = GenerateSchemaUtils.onlyPrintableArgs field.args in
  let writer = CodeWriter.create 512 in
  CodeWriter.line writer "{";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer
        (Printf.sprintf "typ: %s," (printGraphQLType field.typ));
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (field.description |> descriptionAsString));
      CodeWriter.line writer
        (Printf.sprintf "deprecationReason: %s,"
           (field.deprecationReason |> undefinedOrValueAsString));
      if List.length printableArgs > 0 then (
        CodeWriter.add writer "args: ";
        CodeWriter.add writer
          (printArgs ~schemaState ~parentTypeName ~fieldName:field.name
             printableArgs);
        CodeWriter.line writer ",");
      (match
         printDirectiveExtensions schemaState
           (DirectiveFieldDefinition {parentTypeName; fieldName = field.name})
       with
      | None -> ()
      | Some extensions ->
        CodeWriter.line writer (Printf.sprintf "extensions: %s," extensions));
      match context with
      | CtxDefault ->
        CodeWriter.line writer
          (Printf.sprintf "resolve: makeResolveFn(%s)"
             (printResolverForField ~parentTypeName ~schemaState field))
      | CtxInterface -> ()
      | CtxSubscription ->
        CodeWriter.line writer
          (Printf.sprintf
             "resolve: makeResolveFn((v, _, _, _) => v),\n\
              subscribe: makeResolveFn(%s)"
             (printResolverForField ~parentTypeName ~schemaState field)));
  CodeWriter.add writer "}";
  CodeWriter.contents writer

let printInputObjectField ~schemaState ~parentTypeName (field : gqlField) =
  let writer = CodeWriter.create 256 in
  CodeWriter.line writer "{";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer
        (Printf.sprintf "GraphQLInputObjectType.typ: %s,"
           (printGraphQLType field.typ));
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (field.description |> descriptionAsString));
      (match field.defaultValue with
      | None -> ()
      | Some value ->
        CodeWriter.line writer
          (Printf.sprintf "defaultValue: %s," (printConstValue value)));
      CodeWriter.line writer
        (Printf.sprintf "deprecationReason: %s,"
           (field.deprecationReason |> undefinedOrValueAsString));
      match
        printDirectiveExtensions schemaState
          (DirectiveInputFieldDefinition
             {inputObjectName = parentTypeName; fieldName = field.name})
      with
      | None -> ()
      | Some extensions ->
        CodeWriter.line writer (Printf.sprintf "extensions: %s" extensions));
  CodeWriter.add writer "}";
  CodeWriter.contents writer

let printFieldsWith printer (fields : gqlField list) =
  if List.length fields = 0 then "%raw(`{}`)->makeFields"
  else
    let fields =
      fields
      |> List.sort (fun (a1 : gqlField) a2 -> String.compare a1.name a2.name)
    in
    let writer = CodeWriter.create 1024 in
    let lastFieldIndex = List.length fields - 1 in
    CodeWriter.line writer "{";
    CodeWriter.indented writer (fun () ->
        fields
        |> List.iteri (fun index (field : gqlField) ->
            CodeWriter.add writer (Printf.sprintf "\"%s\": " field.name);
            CodeWriter.add writer (printer field);
            CodeWriter.line writer (if index = lastFieldIndex then "" else ",")));
    CodeWriter.add writer "}->makeFields";
    CodeWriter.contents writer

let printFields ?context ~parentTypeName ~schemaState fields =
  printFieldsWith
    (fun field -> printField ?context ~parentTypeName ~schemaState field)
    fields

let printInputObjectFields ~schemaState ~parentTypeName fields =
  printFieldsWith (printInputObjectField ~schemaState ~parentTypeName) fields

let printObjectType ~(schemaState : schemaState) (typ : gqlObjectType) =
  let writer = CodeWriter.create 1024 in
  CodeWriter.line writer "{";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer (Printf.sprintf "name: \"%s\"," typ.displayName);
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (descriptionAsString typ.description));
      CodeWriter.line writer
        (Printf.sprintf "interfaces: [%s],"
           (typ.interfaces |> List.sort String.compare
           |> List.map (fun id ->
               Printf.sprintf "get_%s()"
                 (GenerateSchemaUtils.capitalizeFirstChar id))
           |> String.concat ", "));
      (match
         printDirectiveExtensions schemaState (DirectiveObject typ.displayName)
       with
      | None -> ()
      | Some extensions ->
        CodeWriter.line writer (Printf.sprintf "extensions: %s," extensions));
      CodeWriter.add writer "fields: () => ";
      CodeWriter.add writer
        (printFields
           ?context:
             (if typ.id = "subscription" then Some CtxSubscription else None)
           ~parentTypeName:typ.displayName ~schemaState typ.fields);
      CodeWriter.newline writer);
  CodeWriter.add writer "}";
  CodeWriter.contents writer

let printScalar ~schemaState (typ : gqlScalar) =
  let extensions =
    printDirectiveExtensions schemaState (DirectiveScalar typ.displayName)
  in
  match typ.encoderDecoderLoc with
  | None ->
    Printf.sprintf "{name: \"%s\", description: %s, specifiedByURL: %s%s}"
      typ.displayName
      (descriptionAsString typ.description)
      (undefinedOrValueAsString typ.specifiedByUrl)
      (match extensions with
      | None -> ""
      | Some extensions -> ", extensions: " ^ extensions)
  | Some encoderDecoderLoc ->
    let writer = CodeWriter.create 256 in
    CodeWriter.line writer "{";
    CodeWriter.indented writer (fun () ->
        CodeWriter.line writer
          (Printf.sprintf "let config: GraphQLScalar.config<%s> = {"
             (typeLocationToAccessor typ.typeLocation));
        CodeWriter.indented writer (fun () ->
            CodeWriter.line writer
              (Printf.sprintf "name: \"%s\"," typ.displayName);
            CodeWriter.line writer
              (Printf.sprintf "description: %s,"
                 (descriptionAsString typ.description));
            CodeWriter.line writer
              (Printf.sprintf "specifiedByURL: %s,"
                 (undefinedOrValueAsString typ.specifiedByUrl));
            (match extensions with
            | None -> ()
            | Some extensions ->
              CodeWriter.line writer
                (Printf.sprintf "extensions: %s," extensions));
            CodeWriter.line writer
              (Printf.sprintf "parseValue: %s,"
                 (typeLocationModuleToAccesor encoderDecoderLoc ["parseValue"]));
            CodeWriter.line writer
              (Printf.sprintf "serialize: %s,"
                 (typeLocationModuleToAccesor encoderDecoderLoc ["serialize"])));
        CodeWriter.line writer "}";
        CodeWriter.line writer "config");
    CodeWriter.add writer "}";
    CodeWriter.contents writer

let printInterfaceType ~(schemaState : schemaState) (typ : gqlInterface) =
  let writer = CodeWriter.create 1024 in
  CodeWriter.line writer "{";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer (Printf.sprintf "name: \"%s\"," typ.displayName);
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (descriptionAsString typ.description));
      CodeWriter.line writer
        (Printf.sprintf "interfaces: [%s],"
           (typ.interfaces |> List.sort String.compare
           |> List.map (fun id ->
               Printf.sprintf "get_%s()"
                 (GenerateSchemaUtils.capitalizeFirstChar id))
           |> String.concat ", "));
      (match
         printDirectiveExtensions schemaState
           (DirectiveInterface typ.displayName)
       with
      | None -> ()
      | Some extensions ->
        CodeWriter.line writer (Printf.sprintf "extensions: %s," extensions));
      CodeWriter.add writer "fields: () => ";
      CodeWriter.add writer
        (printFields ~context:CtxInterface ~parentTypeName:typ.displayName
           ~schemaState typ.fields);
      CodeWriter.line writer ",";
      CodeWriter.line writer
        (Printf.sprintf
           "resolveType: \
            GraphQLInterfaceType.makeResolveInterfaceTypeFn(interface_%s_resolveType)"
           typ.displayName));
  CodeWriter.add writer "}";
  CodeWriter.contents writer

let printInputObjectType ~schemaState ?(inputUnion = false)
    (typ : gqlInputObjectType) =
  let writer = CodeWriter.create 512 in
  CodeWriter.line writer "{";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer (Printf.sprintf "name: \"%s\"," typ.displayName);
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (descriptionAsString typ.description));
      CodeWriter.add writer "fields: () => ";
      CodeWriter.add writer
        (printInputObjectFields ~schemaState ~parentTypeName:typ.displayName
           typ.fields);
      if inputUnion then (
        CodeWriter.line writer ",";
        CodeWriter.line writer "isOneOf: true");
      match
        printDirectiveExtensions schemaState
          (DirectiveInputObject typ.displayName)
      with
      | None -> CodeWriter.newline writer
      | Some extensions ->
        if not inputUnion then CodeWriter.line writer ",";
        CodeWriter.line writer (Printf.sprintf "extensions: %s" extensions));
  CodeWriter.add writer "}";
  CodeWriter.contents writer

let printUnionType ~schemaState (union : gqlUnion) =
  let writer = CodeWriter.create 512 in
  CodeWriter.line writer "{";
  CodeWriter.indented writer (fun () ->
      CodeWriter.line writer (Printf.sprintf "name: \"%s\"," union.displayName);
      CodeWriter.line writer
        (Printf.sprintf "description: %s,"
           (descriptionAsString union.description));
      (match
         printDirectiveExtensions schemaState (DirectiveUnion union.displayName)
       with
      | None -> ()
      | Some extensions ->
        CodeWriter.line writer (Printf.sprintf "extensions: %s," extensions));
      CodeWriter.line writer
        (Printf.sprintf "types: () => [%s],"
           (union.types
           |> List.sort (fun (m1 : gqlUnionMember) m2 ->
               String.compare m1.displayName m2.displayName)
           |> List.map (fun (member : gqlUnionMember) ->
               Printf.sprintf "get_%s()" member.displayName)
           |> String.concat ", "));
      CodeWriter.line writer
        (Printf.sprintf
           "resolveType: \
            GraphQLUnionType.makeResolveUnionTypeFn(union_%s_resolveType)"
           union.displayName));
  CodeWriter.add writer "}";
  CodeWriter.contents writer

let getIntfAssets (typ : gqlInterface) ~processedSchema =
  let writer = CodeWriter.create 2048 in
  let interfaceIdentifier = {id = typ.id; displayName = typ.displayName} in
  match Hashtbl.find_opt processedSchema.interfaceImplementedBy typ.id with
  | None -> ""
  | Some implementedBy ->
    CodeWriter.line writer "/* @generated */";
    CodeWriter.blankLine writer;
    CodeWriter.line writer "@@warning(\"-27-34-37\")";
    CodeWriter.blankLine writer;
    CodeWriter.line writer "module Resolver = {";
    CodeWriter.indented writer (fun () ->
        CodeWriter.line writer
          (String.trim
             (printInterfaceResolverReturnType interfaceIdentifier
                ~implementedBy)));
    CodeWriter.line writer "}";
    CodeWriter.blankLine writer;
    CodeWriter.line writer "module ImplementedBy = {";
    CodeWriter.indented writer (fun () ->
        CodeWriter.line writer
          (String.trim (printInterfaceImplementedByType ~implementedBy));
        CodeWriter.blankLine writer;
        CodeWriter.line writer (printInterfaceTypenameDecoder ~implementedBy);
        CodeWriter.blankLine writer;
        CodeWriter.line writer (printInterfaceTypenameToString ~implementedBy));
    CodeWriter.line writer "}";
    if typ.displayName = "Node" then (
      CodeWriter.blankLine writer;
      CodeWriter.line writer (printNodeInterfaceAssets implementedBy));
    CodeWriter.contents writer

let interfaceModuleName ~interfaceModulePrefix intfId =
  match interfaceModulePrefix with
  | None -> Printf.sprintf "Interface_%s" intfId
  | Some prefix -> Printf.sprintf "%s__Interface_%s" prefix intfId

let mkIntfFileName ~interfaceModulePrefix intfId =
  match interfaceModulePrefix with
  | None -> Printf.sprintf "interface_%s.res" intfId
  | Some _ ->
    Printf.sprintf "%s.res" (interfaceModuleName ~interfaceModulePrefix intfId)

let mkIntfFilePath intfId ~outputFolder ~interfaceModulePrefix =
  Printf.sprintf "%s/%s" outputFolder
    (mkIntfFileName ~interfaceModulePrefix intfId)

let printInterfaceFiles (schemaState : schemaState) ~processedSchema
    ~outputFolder ~interfaceModulePrefix =
  schemaState.interfaces
  |> Hashtbl.iter (fun intfId intf ->
      let interfaceFileOutputLoc =
        mkIntfFilePath ~outputFolder ~interfaceModulePrefix intfId
      in
      writeIfHasChanges interfaceFileOutputLoc
        (getIntfAssets intf ~processedSchema))

let cleanInterfaceFiles (schemaState : schemaState) ~outputFolder
    ~interfaceModulePrefix =
  let validNames =
    Hashtbl.fold
      (fun intfId _ acc -> mkIntfFileName ~interfaceModulePrefix intfId :: acc)
      schemaState.interfaces []
  in
  let generatedPrefix =
    match interfaceModulePrefix with
    | None -> "interface_"
    | Some prefix -> prefix ^ "__Interface_"
  in
  let allGeneratedFiles = Array.to_list (Sys.readdir outputFolder) in
  let filesToRemove =
    allGeneratedFiles
    |> List.filter (fun fileName ->
        Filename.check_suffix fileName ".res"
        && String.starts_with
             (Filename.basename fileName)
             ~prefix:generatedPrefix
        && not (List.mem fileName validNames))
  in
  filesToRemove
  |> List.iter (fun fileName ->
      Sys.remove (Filename.concat outputFolder fileName))

let namedSchemaGeneratedHeader = "/* @generated by ResGraph named schema */"

let markNamedSchemaFile contents =
  namedSchemaGeneratedHeader ^ "\n\n" ^ contents

let namedInterfaceFileRegexp = Str.regexp "^.*__Interface_.*\\.res$"

let isLegacySchemaFile fileName =
  fileName = "ResGraphSchema.res" || fileName = "ResGraphSchema.resi"

let isGeneratedInterfaceFile fileName =
  (String.starts_with fileName ~prefix:"interface_"
  || Str.string_match namedInterfaceFileRegexp fileName 0)
  && Filename.check_suffix fileName ".res"

let isNamedSchemaGeneratedFile ~outputFolder fileName =
  if isLegacySchemaFile fileName then true
  else if
    not
      (Filename.check_suffix fileName ".res"
      || Filename.check_suffix fileName ".resi")
  then false
  else
    match Files.readFile (Filename.concat outputFolder fileName) with
    | None -> false
    | Some contents ->
      String.starts_with contents ~prefix:namedSchemaGeneratedHeader
      || isGeneratedInterfaceFile fileName
         && String.starts_with contents ~prefix:"/* @generated */"

let cleanNamedSchemaSdl ~outputFolder =
  let path = Filename.concat outputFolder "schema.graphql" in
  match Files.readFile path with
  | Some contents
    when String.starts_with contents
           ~prefix:"# @generated by ResGraph named schema" ->
    Sys.remove path
  | Some _ | None -> ()

let cleanNamedSchemaFiles ~outputFolder ~moduleName =
  if Sys.file_exists outputFolder && Sys.is_directory outputFolder then
    let currentInterfacePrefix = moduleName ^ "__Interface_" in
    Sys.readdir outputFolder
    |> Array.iter (fun fileName ->
        let belongsToCurrentModule =
          fileName = moduleName ^ ".res"
          || fileName = moduleName ^ ".resi"
          || String.starts_with fileName ~prefix:currentInterfacePrefix
        in
        if
          (not belongsToCurrentModule)
          && isNamedSchemaGeneratedFile ~outputFolder fileName
        then Sys.remove (Filename.concat outputFolder fileName))

exception Interface_not_found of string

let printSchemaJsFile schemaState processSchema ~interfaceModulePrefix =
  let code = CodeWriter.create (1024 * 1024) in
  CodeWriter.line code "@@warning(\"-27-32\")";
  CodeWriter.blankLine code;
  CodeWriter.line code "open ResGraph__GraphQLJs";
  CodeWriter.blankLine code;
  let addWithNewLine = CodeWriter.line code in
  (* Add the type unwrapper. Source types passed to resolvers might be either
     objects or variant cases. This is because we rely on variants for unions
     and interfaces. Variant cases are boxed, so they need to be unwrapped
     before they're passed to the resolver the developer has defined.
     `typeUnwrapper` unwraps any variant case to its specified object.
  *)
  addWithNewLine
    "let typeUnwrapper: ('src) => 'return = %raw(`function typeUnwrapper(src) \
     { if (src == null) return null; if (typeof src === 'object' && \
     src.hasOwnProperty('_0')) return src['_0']; if (typeof src === 'object' \
     && src.hasOwnProperty('VAL')) return src['VAL']; return src;}`)";

  (* Add the input union unwrapper. TODO: Explain more
  *)
  addWithNewLine
    {|let inputUnionUnwrapper: ('src, array<string>, array<string>) => 'return = %raw(`function inputUnionUnwrapper(src, inlineRecordTypenames, emptyPayloadTypenames) {
      if (src == null) return null;

      let targetKey = null;
      let targetValue = null;

      Object.entries(src).forEach(([key, value]) => {
        if (value != null) {
          targetKey = key;
          targetValue = value;
        }
      });

      if (targetKey != null && targetValue != null) {
        let tagName = targetKey.slice(0, 1).toUpperCase() + targetKey.slice(1);

        if (inlineRecordTypenames.includes(tagName)) {
          return Object.assign({ TAG: tagName }, targetValue);
        }

        if (emptyPayloadTypenames.includes(tagName)) {
          return tagName;
        }

        return {
          TAG: tagName,
          _0: targetValue,
        };
      }

      return null;
    }
    `)|};

  addWithNewLine
    {|let resolveInterfaceTypename: ('src, array<string>, string, string) => string = %raw(`function resolveInterfaceTypename(src, allowedTypenames, interfaceName, interfaceResolverTypeName) {
      if (allowedTypenames.length === 1) {
        return allowedTypenames[0];
      }

      if (src != null && typeof src === "object") {
        let tag = src.TAG;

        if (typeof tag === "string" && allowedTypenames.includes(tag)) {
          return tag;
        }

        if (typeof tag === "string") {
          throw new Error(
            "Panic! Interface " +
              interfaceName +
              " resolveType got unexpected TAG " +
              JSON.stringify(tag) +
              ". Expected one of " +
              allowedTypenames.join(", ") +
              ".",
          );
        }
      }

      throw new Error(
        "Panic! Interface " +
          interfaceName +
          " resolveType expected a tagged value from " +
          interfaceResolverTypeName +
          ", but got an untagged value. Use " +
          interfaceResolverTypeName +
          " for interface return values instead of the bare interface record type.",
      );
    }
    `)|};

  (* Add conversion assets. *)
  addWithNewLine "";
  addWithNewLine "type inputObjectFieldConverterFn";
  addWithNewLine
    "external makeInputObjectFieldConverterFn: ('a => 'b) => \
     inputObjectFieldConverterFn = \"%identity\"";
  addWithNewLine "";
  addWithNewLine
    {|let applyConversionToInputObject: ('a, array<(string, inputObjectFieldConverterFn)>) => 'a = %raw(`function applyConversionToInputObject(obj, instructions) {
  if (instructions.length === 0) return obj;
  let newObj = Object.assign({}, obj);
  instructions.forEach(instruction => {
    let value = newObj[instruction[0]];
    newObj[instruction[0]] = instruction[1](value);
  })
  return newObj;
}`)|};
  addWithNewLine "";

  (* Print all custom scalars. *)
  schemaState.scalars
  |> iterHashtblAlphabetically (fun _name (scalar : gqlScalar) ->
      addWithNewLine
        (Printf.sprintf "let scalar_%s = GraphQLScalar.make(%s)"
           scalar.displayName
           (printScalar ~schemaState scalar)));
  addWithNewLine "";

  (* Print all enums. These won't have any other dependencies. *)
  schemaState.enums
  |> iterHashtblAlphabetically (fun _name (enum : gqlEnum) ->
      CodeWriter.line code
        (Printf.sprintf "let enum_%s = GraphQLEnumType.make({" enum.displayName);
      CodeWriter.indented code (fun () ->
          CodeWriter.line code (Printf.sprintf "name: \"%s\"," enum.displayName);
          CodeWriter.line code
            (Printf.sprintf "description: %s,"
               (descriptionAsString enum.description));
          (match
             printDirectiveExtensions schemaState
               (DirectiveEnum enum.displayName)
           with
          | None -> ()
          | Some extensions ->
            CodeWriter.line code (Printf.sprintf "extensions: %s," extensions));
          CodeWriter.line code "values: {";
          CodeWriter.indented code (fun () ->
              enum.values
              |> List.iter (fun (value : gqlEnumValue) ->
                  let extensions =
                    printDirectiveExtensions schemaState
                      (DirectiveEnumValue
                         {enumName = enum.displayName; valueName = value.value})
                  in
                  CodeWriter.line code
                    (Printf.sprintf
                       "\"%s\": {GraphQLEnumType.value: \"%s\", description: \
                        %s, deprecationReason: %s%s},"
                       value.value value.value
                       (descriptionAsString value.description)
                       (undefinedOrValueAsString value.deprecationReason)
                       (match extensions with
                       | None -> ""
                       | Some extensions -> ", extensions: " ^ extensions))));
          CodeWriter.line code "}->makeEnumValues,");
      CodeWriter.line code "})";
      CodeWriter.blankLine code);

  (* Print the interface type holders and getters *)
  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (typ : gqlInterface) ->
      addWithNewLine
        (Printf.sprintf
           "let i_%s: ref<GraphQLInterfaceType.t> = Obj.magic({\"contents\": \
            null})"
           typ.displayName);
      addWithNewLine
        (Printf.sprintf "let get_%s = () => i_%s.contents" typ.displayName
           typ.displayName));

  (* Print the object type holders and getters *)
  schemaState.types
  |> iterHashtblAlphabetically (fun _ (typ : gqlObjectType) ->
      addWithNewLine
        (Printf.sprintf
           "let t_%s: ref<GraphQLObjectType.t> = Obj.magic({\"contents\": \
            null})"
           typ.displayName);
      addWithNewLine
        (Printf.sprintf "let get_%s = () => t_%s.contents" typ.displayName
           typ.displayName));

  (* Print the input union type holders and getters *)
  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (inputUnion : gqlInputUnionType) ->
      addWithNewLine
        (Printf.sprintf
           "let inputUnion_%s: ref<GraphQLInputObjectType.t> = \
            Obj.magic({\"contents\": null})"
           inputUnion.displayName);
      addWithNewLine
        (Printf.sprintf "let get_%s = () => inputUnion_%s.contents"
           inputUnion.displayName inputUnion.displayName);
      addWithNewLine
        (Printf.sprintf "let inputUnion_%s_conversionInstructions = []"
           inputUnion.displayName));

  (* Print the input object type holders and getters *)
  schemaState.inputObjects
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputObjectType) ->
      addWithNewLine
        (Printf.sprintf
           "let input_%s: ref<GraphQLInputObjectType.t> = \
            Obj.magic({\"contents\": null})"
           typ.displayName);
      addWithNewLine
        (Printf.sprintf "let get_%s = () => input_%s.contents" typ.displayName
           typ.displayName);
      addWithNewLine
        (Printf.sprintf "let input_%s_conversionInstructions = []"
           typ.displayName));

  (* Now add all of the conversion instructions. *)
  schemaState.inputObjects
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputObjectType) ->
      addWithNewLine (printInputObjectAssets typ));

  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputUnionType) ->
      addWithNewLine (typ |> printInputUnionAssets));

  (* Print the union type holders and getters *)
  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
      addWithNewLine
        (Printf.sprintf
           "let union_%s: ref<GraphQLUnionType.t> = Obj.magic({\"contents\": \
            null})"
           union.displayName);
      addWithNewLine
        (Printf.sprintf "let get_%s = () => union_%s.contents" union.displayName
           union.displayName));

  addWithNewLine "";

  (* Print support functions for union type resolution *)
  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
      addWithNewLine
        (Printf.sprintf "let union_%s_resolveType = (v%s) => switch v {%s}\n"
           union.displayName
           (match union.typeLocation with
           | Synthetic _ -> ""
           | Concrete typeLocation -> ": " ^ typeLocationToAccessor typeLocation)
           (union.types
           |> List.map (fun (member : gqlUnionMember) ->
               Printf.sprintf " | %s%s(_) => \"%s\""
                 (if union.typeSource = Polyvariant then "#" else "")
                 member.constructorName member.displayName)
           |> String.concat "\n")));

  (* Print support functions for interface type resolution *)
  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (intf : gqlInterface) ->
      (* TODO: Flatten list properly when constructing *)
      let implementedBy =
        match Hashtbl.find_opt processSchema.interfaceImplementedBy intf.id with
        | Some i -> i
        | None -> raise (Interface_not_found ("Interface: " ^ intf.id))
      in
      let resolvedTypenames =
        implementedBy
        |> List.sort sortImplementedBy
        |> List.map (fun (member : interfaceImplementedBy) ->
            match member with
            | ObjectType {displayName} | Interface {displayName} ->
              Printf.sprintf "\"%s\"" displayName)
        |> String.concat ", "
      in
      let interfaceModule =
        interfaceModuleName ~interfaceModulePrefix intf.id
      in
      addWithNewLine
        (Printf.sprintf
           "let interface_%s_resolveType = (v: %s.Resolver.t) => \
            resolveInterfaceTypename(v, [%s], \"%s\", \"%s.Resolver.t\")\n"
           intf.displayName interfaceModule resolvedTypenames intf.displayName
           interfaceModule));

  (* Now we can print all of the code that fills these in. *)
  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (typ : gqlInterface) ->
      addWithNewLine
        (Printf.sprintf "i_%s.contents = GraphQLInterfaceType.make(%s)"
           typ.displayName
           (typ |> printInterfaceType ~schemaState)));

  schemaState.types
  |> iterHashtblAlphabetically (fun _name (typ : gqlObjectType) ->
      addWithNewLine
        (Printf.sprintf "t_%s.contents = GraphQLObjectType.make(%s)"
           typ.displayName
           (typ |> printObjectType ~schemaState)));

  schemaState.inputObjects
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputObjectType) ->
      addWithNewLine
        (Printf.sprintf "input_%s.contents = GraphQLInputObjectType.make(%s)"
           typ.displayName
           (typ |> printInputObjectType ~schemaState)));

  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputUnionType) ->
      addWithNewLine
        (Printf.sprintf
           "inputUnion_%s.contents = GraphQLInputObjectType.make(%s)"
           typ.displayName
           (typ |> inputUnionToInputObj
           |> printInputObjectType ~schemaState ~inputUnion:true)));

  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
      addWithNewLine
        (Printf.sprintf "union_%s.contents = GraphQLUnionType.make(%s)"
           union.displayName
           (union |> printUnionType ~schemaState)));

  if Hashtbl.length schemaState.directiveDefinitions > 0 then
    CodeWriter.blankLine code;
  schemaState.directiveDefinitions
  |> iterHashtblAlphabetically
       (fun _name (definition : gqlDirectiveDefinition) ->
         addWithNewLine
           (Printf.sprintf "let directive_%s = GraphQLDirective.make(%s)"
              definition.name
              (printDirectiveDefinition schemaState definition)));

  (* Print the schema gluing it all together. *)
  let customDirectives =
    hashtblToListAlphabetically schemaState.directiveDefinitions
    |> List.map (fun (_name, (definition : gqlDirectiveDefinition)) ->
        "directive_" ^ definition.name)
  in
  let schemaTypes =
    (hashtblToListAlphabetically schemaState.types
    |> List.map (fun (_name, (typ : gqlObjectType)) ->
        "get_" ^ typ.displayName ^ "()->GraphQLObjectType.toGraphQLType"))
    @ (hashtblToListAlphabetically schemaState.interfaces
      |> List.map (fun (_name, (typ : gqlInterface)) ->
          "get_" ^ typ.displayName ^ "()->GraphQLInterfaceType.toGraphQLType"))
    @ (hashtblToListAlphabetically schemaState.unions
      |> List.map (fun (_name, (typ : gqlUnion)) ->
          "get_" ^ typ.displayName ^ "()->GraphQLUnionType.toGraphQLType"))
    @ (hashtblToListAlphabetically schemaState.inputUnions
      |> List.map (fun (_name, (typ : gqlInputUnionType)) ->
          "get_" ^ typ.displayName ^ "()->GraphQLInputObjectType.toGraphQLType")
      )
    @ (hashtblToListAlphabetically schemaState.inputObjects
      |> List.map (fun (_name, (typ : gqlInputObjectType)) ->
          "get_" ^ typ.displayName ^ "()->GraphQLInputObjectType.toGraphQLType")
      )
    @ (hashtblToListAlphabetically schemaState.enums
      |> List.map (fun (_name, (typ : gqlEnum)) ->
          "enum_" ^ typ.displayName ^ "->GraphQLEnumType.toGraphQLType"))
  in
  let lastSchemaTypeIndex = List.length schemaTypes - 1 in
  CodeWriter.blankLine code;
  CodeWriter.line code "let schema = GraphQLSchemaType.makeConfig({";
  CodeWriter.indented code (fun () ->
      (match schemaState.schemaDefinition with
      | Some {description = Some description} ->
        CodeWriter.line code (Printf.sprintf "description: %S," description)
      | Some _ | None -> ());
      (match schemaState.query with
      | None -> ()
      | Some query ->
        CodeWriter.line code
          (Printf.sprintf "query: get_%s()," query.displayName));
      (match schemaState.mutation with
      | None -> ()
      | Some mutation ->
        CodeWriter.line code
          (Printf.sprintf "mutation: get_%s()," mutation.displayName));
      (match schemaState.subscription with
      | None -> ()
      | Some subscription ->
        CodeWriter.line code
          (Printf.sprintf "subscription: get_%s()," subscription.displayName));
      if customDirectives <> [] then
        CodeWriter.line code
          (Printf.sprintf
             "directives: [...GraphQLDirective.specifiedDirectives, %s],"
             (String.concat ", " customDirectives));
      (match printDirectiveExtensions schemaState DirectiveSchema with
      | None -> ()
      | Some extensions ->
        CodeWriter.line code (Printf.sprintf "extensions: %s," extensions));
      CodeWriter.line code "types: [";
      CodeWriter.indented code (fun () ->
          schemaTypes
          |> List.iteri (fun index schemaType ->
              CodeWriter.line code
                (schemaType ^ if index = lastSchemaTypeIndex then "" else ",")));
      CodeWriter.line code "]");
  CodeWriter.line code "})";
  CodeWriter.contents code
