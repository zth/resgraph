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
  | GraphQLInterface {displayName} ->
    Printf.sprintf "get_%s()->GraphQLInterfaceType.toGraphQLType%s" displayName
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

let printInterfaceResolverReturnType
    (gqlInterfaceIdentifier : gqlInterfaceIdentifier)
    ~(implementedBy : interfaceImplementedBy list) =
  Printf.sprintf "@gql.interfaceResolver(\"%s\") type %s_resolver = %s"
    gqlInterfaceIdentifier.id gqlInterfaceIdentifier.id
    (implementedBy
    |> List.map (fun (i : interfaceImplementedBy) ->
           match i with
           | ObjectType t ->
             Printf.sprintf "%s(%s)" t.displayName
               (typeLocationToAccessor t.typeLocation)
           | Interface t ->
             Printf.sprintf "%s(%s)" t.displayName
               (typeLocationToAccessor t.typeLocation))
    |> String.concat " | ")

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
  Printf.sprintf
    "{name: \"%s\", description: %s, interfaces: [%s], fields: () => %s}"
    typ.displayName
    (undefinedOrValueAsString typ.description)
    (typ.interfaces
    |> List.map (fun (item : gqlInterfaceIdentifier) ->
           Printf.sprintf "get_%s()" item.displayName)
    |> String.concat ", ")
    (printFields typ.fields)

let printInterfaceType (typ : gqlInterface) =
  Printf.sprintf
    "{name: \"%s\", description: %s, interfaces: [%s], fields: () => %s, \
     resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(%s)}"
    typ.displayName
    (undefinedOrValueAsString typ.description)
    (typ.interfaces
    |> List.map (fun (item : gqlInterfaceIdentifier) ->
           Printf.sprintf "get_%s()" item.displayName)
    |> String.concat ", ")
    (printFields typ.fields)
    (Printf.sprintf "interface_%s_resolveType" typ.displayName)

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

let printSchemaAssets ~schemaState ~processedSchema =
  let code = ref "@@warning(\"-27-34-37\")\n\n" in
  let addWithNewLine text = code := !code ^ text ^ "\n" in
  schemaState.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
         match
           Hashtbl.find_opt processedSchema.interfaceImplementedBy typ.id
         with
         | None -> ()
         | Some implementedBy ->
           addWithNewLine
             (printInterfaceResolverReturnType
                {id = typ.id; displayName = typ.displayName}
                ~implementedBy));
  !code

let printSchemaJsFile schemaState processSchema =
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
  schemaState.enums
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

  (* Print the interface type holders and getters *)
  schemaState.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
         addWithNewLine
           (Printf.sprintf
              "let i_%s: ref<GraphQLInterfaceType.t> = \
               Obj.magic({\"contents\": Js.null})"
              typ.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => i_%s.contents" typ.displayName
              typ.displayName));

  (* Print the object type holders and getters *)
  schemaState.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
         addWithNewLine
           (Printf.sprintf
              "let t_%s: ref<GraphQLObjectType.t> = Obj.magic({\"contents\": \
               Js.null})"
              typ.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => t_%s.contents" typ.displayName
              typ.displayName));

  (* Print the input object type holders and getters *)
  schemaState.inputObjects
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
  schemaState.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
         addWithNewLine (printInputObjectAssets typ));

  (* Print the union type holders and getters *)
  schemaState.unions
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
  schemaState.unions
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

  (* Print support functions for interface type resolution *)
  schemaState.interfaces
  |> Hashtbl.iter (fun _name (intf : gqlInterface) ->
         (* TODO: Flatten list properly when constructing *)
         let implementedBy =
           Hashtbl.find processSchema.interfaceImplementedBy intf.id
         in
         addWithNewLine
           (Printf.sprintf
              "let interface_%s_resolveType = (v: \
               ResGraphSchemaAssets.%s_resolver) => switch v {%s}\n"
              intf.displayName intf.id
              (implementedBy
              |> List.map (fun (member : interfaceImplementedBy) ->
                     let displayName =
                       match member with
                       | ObjectType {displayName} | Interface {displayName} ->
                         displayName
                     in
                     Printf.sprintf " | %s(_) => get_%s()" displayName
                       displayName)
              |> String.concat "\n")));

  (* Now we can print all of the code that fills these in. *)
  schemaState.interfaces
  |> Hashtbl.iter (fun _name (typ : gqlInterface) ->
         addWithNewLine
           (Printf.sprintf "i_%s.contents = GraphQLInterfaceType.make(%s)"
              typ.displayName
              (typ |> printInterfaceType)));

  schemaState.types
  |> Hashtbl.iter (fun _name (typ : gqlObjectType) ->
         addWithNewLine
           (Printf.sprintf "t_%s.contents = GraphQLObjectType.make(%s)"
              typ.displayName (typ |> printObjectType)));

  schemaState.inputObjects
  |> Hashtbl.iter (fun _name (typ : gqlInputObjectType) ->
         addWithNewLine
           (Printf.sprintf "input_%s.contents = GraphQLInputObjectType.make(%s)"
              typ.displayName
              (typ |> printInputObjectType)));

  schemaState.unions
  |> Hashtbl.iter (fun _name (union : gqlUnion) ->
         addWithNewLine
           (Printf.sprintf "union_%s.contents = GraphQLUnionType.make(%s)"
              union.displayName (union |> printUnionType)));

  (* Print the schema gluing it all together. *)
  addWithNewLine "";
  addWithNewLine
    (Printf.sprintf
       "let schema = GraphQLSchemaType.make({\"query\": get_Query()%s%s})"
       (match schemaState.mutation with
       | None -> ""
       | Some _ -> ", \"mutation\": get_Mutation()")
       (match schemaState.subscription with
       | None -> ""
       | Some _ -> ", \"subscription\": get_Subscription()"));
  !code
