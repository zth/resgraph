open GenerateSchemaTypes
open GenerateSchemaUtils

let printResolverForField (field : gqlField) =
  match field.resolverStyle with
  | Property name ->
    Printf.sprintf
      "(src, _args, _ctx) => {let src = typeUnwrapper(src); src[\"%s\"]}" name
  | Resolver {moduleName; fnName; pathToFn} ->
    let resolverCode =
      Printf.sprintf "(src, args, ctx) => {let src = typeUnwrapper(src); %s(src"
        ([moduleName] @ pathToFn @ [fnName] |> String.concat ".")
    in
    if List.length field.args > 0 then
      resolverCode ^ ", "
      ^ (field.args
        |> List.map (fun (arg : gqlArg) ->
               Printf.sprintf "~%s=args[\"%s\"]%s" arg.name arg.name
                 (if argIsOptional arg then
                  (* TODO: Convert lists and nullables too when appropriate *)
                  "->Js.Nullable.toOption"
                 else ""))
        |> String.concat ", ")
      ^ ")}"
    else resolverCode ^ ")}"
let rec printGraphQLType ?(nullable = false) (returnType : graphqlType) =
  let nullablePrefix = if nullable then "" else "->nonNull" in
  match returnType with
  | List inner ->
    Printf.sprintf "GraphQLListType.make(%s)->GraphQLListType.toGraphQLType"
      (printGraphQLType ~nullable:true inner)
  | Nullable inner -> printGraphQLType ~nullable:true inner
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
      nullablePrefix
  | GraphQLObjectType {name} ->
    Printf.sprintf "get_%s()->GraphQLObjectType.toGraphQLType%s" name
      nullablePrefix
  | GraphQLEnum {name} ->
    Printf.sprintf "enum_%s->GraphQLEnumType.toGraphQLType%s" name
      nullablePrefix
  | GraphQLUnion {name} ->
    Printf.sprintf "get_%s()->GraphQLUnionType.toGraphQLType%s" name
      nullablePrefix
  | Named {path} ->
    Printf.printf "Named! %s\n" (SharedTypes.pathIdentToString path);
    "Obj.magic()"

let printArg (arg : gqlArg) =
  Printf.sprintf "{typ: %s}" (printGraphQLType arg.typ)
let printArgs (args : gqlArg list) =
  args
  |> List.map (fun (arg : gqlArg) ->
         Printf.sprintf "\"%s\": %s" arg.name (printArg arg))
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
let printFields (fields : gqlField list) =
  Printf.sprintf "{%s}->makeFields"
    (fields
    |> List.map (fun (field : gqlField) ->
           Printf.sprintf "\"%s\": %s" field.name (printField field))
    |> String.concat ",\n")
let printObjectType (typ : gqlObjectType) =
  Printf.sprintf "{name: \"%s\", description: %s, fields: () => %s}" typ.name
    (undefinedOrValueAsString typ.description)
    (printFields typ.fields)

let printUnionType (union : gqlUnion) =
  Printf.sprintf
    "{name: \"%s\", description: %s, types: () => [%s], resolveType: \
     GraphQLUnionType.makeResolveUnionTypeFn(%s)}"
    union.name
    (undefinedOrValueAsString union.description)
    (union.types
    |> List.map (fun (member : gqlUnionMember) ->
           Printf.sprintf "get_%s()" member.objectTypeName)
    |> String.concat ", ")
    (Printf.sprintf "union_%s_resolveType" union.name)
