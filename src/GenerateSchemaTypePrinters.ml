open GenerateSchemaTypes
open GenerateSchemaUtils

let printResolverForField (field : gqlField) =
  match field.resolverStyle with
  | Property name ->
    Printf.sprintf
      "(src, _args, _ctx) => {let src = typeUnwrapper(src); src[\"%s\"]}" name
  | Resolver {moduleName; fnName; pathToFn} ->
    let ctxArgName = findContextArgName field.args in
    let hasCtxArg = ctxArgName |> Option.is_some in
    let resolverCode =
      Printf.sprintf "(src, args, ctx) => {let src = typeUnwrapper(src); %s(src"
        ([moduleName] @ pathToFn @ [fnName] |> String.concat ".")
    in
    if List.length field.args > 0 then
      resolverCode ^ ", "
      ^ (field.args
        |> List.map (fun (arg : gqlArg) ->
               if hasCtxArg && Some arg.name = ctxArgName then
                 Printf.sprintf "~%s=ctx" (ctxArgName |> Option.get)
               else
                 let argsText =
                   generateConverter
                     (Printf.sprintf "args[\"%s\"]" arg.name)
                     arg.typ
                 in
                 Printf.sprintf "~%s=%s" arg.name
                   (if arg.isOptionLabelled then Printf.sprintf "?(%s)" argsText
                   else argsText))
        |> String.concat ", ")
      ^ ")}"
    else resolverCode ^ ")}"
let rec printGraphQLType ?(nullable = false) (returnType : graphqlType) =
  let nullablePostfix = if nullable then "" else "->nonNull" in
  match returnType with
  | List inner ->
    Printf.sprintf "GraphQLListType.make(%s)->GraphQLListType.toGraphQLType%s"
      (printGraphQLType inner) nullablePostfix
  | RescriptNullable inner | Nullable inner ->
    printGraphQLType ~nullable:true inner
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
  | GraphQLInputObject {displayName} ->
    Printf.sprintf "get_%s()->GraphQLInputObjectType.toGraphQLType%s"
      displayName nullablePostfix
  | GraphQLEnum {displayName} ->
    Printf.sprintf "enum_%s->GraphQLEnumType.toGraphQLType%s" displayName
      nullablePostfix
  | GraphQLUnion {displayName} ->
    Printf.sprintf "get_%s()->GraphQLUnionType.toGraphQLType%s" displayName
      nullablePostfix
  | InjectContext -> "Obj.magic()"
  | Named {path} ->
    Printf.printf "Named! %s\n" (SharedTypes.pathIdentToString path);
    "Obj.magic()"

let printArg (arg : gqlArg) =
  Printf.sprintf "{typ: %s}" (printGraphQLType arg.typ)
let printArgs (args : gqlArg list) =
  args
  |> List.filter_map (fun (arg : gqlArg) ->
         if arg.typ = InjectContext then None
         else Some (Printf.sprintf "\"%s\": %s" arg.name (printArg arg)))
  |> String.concat ", "
let printField (field : gqlField) =
  Printf.sprintf
    "{typ: %s, description: %s, deprecationReason: %s, %sresolve: \
     makeResolveFn(%s)}"
    (printGraphQLType field.typ)
    (field.description |> undefinedOrValueAsString)
    (field.deprecationReason |> undefinedOrValueAsString)
    (if field.args |> List.length > 0 then
     Printf.sprintf " args: {%s}->makeArgs, " (printArgs field.args)
    else " ")
    (printResolverForField field)

let printInputObjectField (field : gqlField) =
  Printf.sprintf
    "{GraphQLInputObjectType.typ: %s, description: %s, deprecationReason: %s}"
    (printGraphQLType field.typ)
    (field.description |> undefinedOrValueAsString)
    (field.deprecationReason |> undefinedOrValueAsString)

let printFields (fields : gqlField list) =
  Printf.sprintf "{%s}->makeFields"
    (if fields |> List.length = 0 then "%raw(`{}`)"
    else
      fields
      |> List.map (fun (field : gqlField) ->
             Printf.sprintf "\"%s\": %s" field.name (printField field))
      |> String.concat ",\n")
let printInputObjectFields (fields : gqlField list) =
  Printf.sprintf "{%s}->makeFields"
    (if fields |> List.length = 0 then "%raw(`{}`)"
    else
      fields
      |> List.map (fun (field : gqlField) ->
             Printf.sprintf "\"%s\": %s" field.name
               (printInputObjectField field))
      |> String.concat ",\n")
let printObjectType (typ : gqlObjectType) =
  Printf.sprintf "{name: \"%s\", description: %s, fields: () => %s}"
    typ.displayName
    (undefinedOrValueAsString typ.description)
    (printFields typ.fields)

let printInputObjectType (typ : gqlInputObjectType) =
  Printf.sprintf "{name: \"%s\", description: %s, fields: () => %s}"
    typ.displayName
    (undefinedOrValueAsString typ.description)
    (printInputObjectFields typ.fields)

let printUnionType (union : gqlUnion) =
  Printf.sprintf
    "{name: \"%s\", description: %s, types: () => [%s], resolveType: \
     GraphQLUnionType.makeResolveUnionTypeFn(%s)}"
    union.displayName
    (undefinedOrValueAsString union.description)
    (union.types
    |> List.map (fun (member : gqlUnionMember) ->
           Printf.sprintf "get_%s()" member.displayName)
    |> String.concat ", ")
    (Printf.sprintf "union_%s_resolveType" union.displayName)
