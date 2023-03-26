open GenerateSchemaTypes

let printResolverForField ~state (field : gqlField) =
  match field.resolverStyle with
  | Property name -> Printf.sprintf "(src, _args, _ctx) => src[\"%s\"]" name
  | Resolver {moduleName; fnName; pathToFn} ->
    let resolverCode =
      Printf.sprintf "(src, args, ctx) => {%s(src"
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
let rec printReturnType ~state ?(nullable = false) (returnType : returnType) =
  let nullablePrefix = if nullable then "" else "->nonNull" in
  match returnType with
  | Nullable inner -> printReturnType ~state ~nullable:true inner
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
  | Named {path} ->
    Printf.printf "Named! %s\n" (SharedTypes.pathIdentToString path);
    "Obj.magic()"
let printField ~state (field : gqlField) =
  Printf.sprintf "{typ: %s, resolve: makeResolveFn(%s)}"
    (printReturnType ~state field.typ)
    (printResolverForField ~state field)
let printFields ~state (fields : gqlField list) =
  Printf.sprintf "{%s}->makeFields"
    (fields
    |> List.map (fun (field : gqlField) ->
           Printf.sprintf "\"%s\": %s" field.name (printField ~state field))
    |> String.concat ",\n")
let printObjectType ~state (typ : typ) =
  Printf.sprintf "{name: \"%s\", fields: () => %s}" typ.name
    (printFields typ.fields ~state)