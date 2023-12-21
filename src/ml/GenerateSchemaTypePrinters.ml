open GenerateSchemaTypes
open GenerateSchemaUtils

type context = CtxDefault | CtxInterface

let printResolverForField (field : gqlField) =
  match field.resolverStyle with
  | Property name ->
    Printf.sprintf
      "(src, _args, _ctx) => {let src = typeUnwrapper(src); src[\"%s\"]}" name
  | Resolver {moduleName; fnName; pathToFn} ->
    let ctxArgName = findContextArgName field.args in
    let hasCtxArg = ctxArgName |> Option.is_some in

    let intfTypeArgName = findInterfaceTypeArgName field.args in
    let hasIntTypeArg = intfTypeArgName |> Option.is_some in
    let resolverCode =
      Printf.sprintf "(src, args, ctx) => {let src = typeUnwrapper(src); %s(src"
        ([moduleName] @ pathToFn @ [fnName] |> String.concat ".")
    in
    if List.length field.args > 0 then
      resolverCode ^ ", "
      ^ (field.args
        |> List.sort (fun (a1 : gqlArg) a2 -> String.compare a1.name a2.name)
        |> List.map (fun (arg : gqlArg) ->
               if hasCtxArg && Some arg.name = ctxArgName then
                 Printf.sprintf "~%s=ctx" (ctxArgName |> Option.get)
               else if hasIntTypeArg && Some arg.name = intfTypeArgName then
                 match field.onType with
                 | Some name ->
                   Printf.sprintf "~%s=%s" (intfTypeArgName |> Option.get) name
                 | _ -> ""
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
  | GraphQLScalar {displayName} ->
    Printf.sprintf "scalar_%s->GraphQLScalar.toGraphQLType%s" displayName
      nullablePostfix
  | GraphQLInterface {displayName} ->
    Printf.sprintf "get_%s()->GraphQLInterfaceType.toGraphQLType%s" displayName
      nullablePostfix
  | InjectInterfaceTypename intfId ->
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
  | InjectContext -> "Obj.magic()"

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
  | ObjectType {typeLocation = Some typeLocation} | Interface {typeLocation} ->
    typeLocationToAccessor typeLocation
  | ObjectType {typeLocation = None} -> raise (Failure "Error code: TLFIB_MTL")

let sortImplementedBy (a1 : interfaceImplementedBy) a2 =
  String.compare
    (displayNameFromImplementedBy a1)
    (displayNameFromImplementedBy a2)

let printInterfaceResolverReturnType
    (gqlInterfaceIdentifier : gqlInterfaceIdentifier)
    ~(implementedBy : interfaceImplementedBy list) =
  Printf.sprintf " @gql.interfaceResolver(\"%s\") type t = %s"
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
             Printf.sprintf "@as(\"%s\") %s: 'a,"
               (displayNameFromImplementedBy i)
               (idFromImplementedBy i))
      |> String.concat "\n  ")
    ^ Printf.sprintf
        {|module TypeMap: {
  type t<'value>
  let make: (typeMap<'value>, ~valueToString: 'value => string) => t<'value>

  /** Takes a (stringified) value and returns what type it represents, if any. */
  let getTypeByStringifiedValue: (t<'value>, string) => option<ImplementedBy.t>

  /** Takes a type and returns what value it represents, as string. */
  let getStringifiedValueByType: (t<'value>, ImplementedBy.t) => string
} = {
  external unsafe_toDict: typeMap<'value> => Js.Dict.t<'value> = "%%identity"
  external unsafe_toType: string => ImplementedBy.t = "%%identity"
  type t<'value> = {
    typeToValue: Js.Dict.t<'value>,
    valueToTypeAsString: Js.Dict.t<string>,
    valueToString: 'value => string,
  }
  let make = (typeMap, ~valueToString) => {
    typeToValue: typeMap->unsafe_toDict,
    valueToTypeAsString: typeMap
    ->unsafe_toDict
    ->Js.Dict.entries
    ->Array.map(((key, value)) => (valueToString(value), key))
    ->Js.Dict.fromArray,
    valueToString,
  }

  let getStringifiedValueByType = (t, typ) =>
    t.typeToValue->Dict.get(typ->ImplementedBy.toString)->Option.getExn->t.valueToString
  let getTypeByStringifiedValue = (t, str) =>
    t.valueToTypeAsString->Dict.get(str)->Option.map(unsafe_toType)
}|}

let printInterfaceTypenameDecoder ~(implementedBy : interfaceImplementedBy list)
    =
  if List.length implementedBy = 0 then ""
  else
    Printf.sprintf
      "let decode = (str: string) => switch str { | %s | _ => None}"
      (implementedBy
      |> List.sort sortImplementedBy
      |> List.map (fun (i : interfaceImplementedBy) ->
             let displayName = displayNameFromImplementedBy i in
             Printf.sprintf "\"%s\" => Some(%s)" displayName displayName)
      |> String.concat " | ")

let printInterfaceTypenameToString
    ~(implementedBy : interfaceImplementedBy list) =
  if List.length implementedBy = 0 then ""
  else Printf.sprintf "external toString: t => string = \"%%identity\""

let printArg (arg : gqlArg) =
  Printf.sprintf "{typ: %s}" (printGraphQLType arg.typ)
let printArgs (args : gqlArg list) =
  args
  |> List.sort (fun (a1 : gqlArg) a2 -> String.compare a1.name a2.name)
  |> List.filter_map (fun (arg : gqlArg) ->
         if isPrintableArg arg then
           Some (Printf.sprintf "\"%s\": %s" arg.name (printArg arg))
         else None)
  |> String.concat ", "
let printField ?(context = CtxDefault) (field : gqlField) =
  let printableArgs = GenerateSchemaUtils.onlyPrintableArgs field.args in
  Printf.sprintf "{typ: %s, description: %s, deprecationReason: %s, %s%s}"
    (printGraphQLType field.typ)
    (field.description |> descriptionAsString)
    (field.deprecationReason |> undefinedOrValueAsString)
    (if printableArgs |> List.length > 0 then
     Printf.sprintf " args: {%s}->makeArgs, " (printArgs printableArgs)
    else " ")
    (match context with
    | CtxDefault ->
      Printf.sprintf "resolve: makeResolveFn(%s)" (printResolverForField field)
    | CtxInterface -> "")

let printInputObjectField (field : gqlField) =
  Printf.sprintf
    "{GraphQLInputObjectType.typ: %s, description: %s, deprecationReason: %s}"
    (printGraphQLType field.typ)
    (field.description |> descriptionAsString)
    (field.deprecationReason |> undefinedOrValueAsString)

let printFields ?context (fields : gqlField list) =
  Printf.sprintf "{%s}->makeFields"
    (if fields |> List.length = 0 then "%raw(`{}`)"
    else
      fields
      |> List.sort (fun (a1 : gqlField) a2 -> String.compare a1.name a2.name)
      |> List.map (fun (field : gqlField) ->
             Printf.sprintf "\"%s\": %s" field.name (printField ?context field))
      |> String.concat ",\n")
let printInputObjectFields (fields : gqlField list) =
  Printf.sprintf "{%s}->makeFields"
    (if fields |> List.length = 0 then "%raw(`{}`)"
    else
      fields
      |> List.sort (fun (a1 : gqlField) a2 -> String.compare a1.name a2.name)
      |> List.map (fun (field : gqlField) ->
             Printf.sprintf "\"%s\": %s" field.name
               (printInputObjectField field))
      |> String.concat ",\n")
let printObjectType (typ : gqlObjectType) =
  Printf.sprintf
    "{name: \"%s\", description: %s, interfaces: [%s], fields: () => %s}"
    typ.displayName
    (descriptionAsString typ.description)
    (typ.interfaces |> List.sort String.compare
    |> List.map (fun id ->
           Printf.sprintf "get_%s()"
             (GenerateSchemaUtils.capitalizeFirstChar id))
    |> String.concat ", ")
    (printFields typ.fields)

let printScalar (typ : gqlScalar) =
  match typ.encoderDecoderLoc with
  | None ->
    Printf.sprintf "{ name: \"%s\", description: %s}" typ.displayName
      (descriptionAsString typ.description)
  | Some encoderDecoderLoc ->
    Printf.sprintf
      "{ let config: GraphQLScalar.config<%s> = {name: \"%s\", description: \
       %s, parseValue: %s, serialize: %s}; config}"
      (typeLocationToAccessor typ.typeLocation)
      typ.displayName
      (descriptionAsString typ.description)
      (typeLocationModuleToAccesor encoderDecoderLoc ["parseValue"])
      (typeLocationModuleToAccesor encoderDecoderLoc ["serialize"])

let printInterfaceType (typ : gqlInterface) =
  Printf.sprintf
    "{name: \"%s\", description: %s, interfaces: [%s], fields: () => %s, \
     resolveType: GraphQLInterfaceType.makeResolveInterfaceTypeFn(%s)}"
    typ.displayName
    (descriptionAsString typ.description)
    (typ.interfaces |> List.sort String.compare
    |> List.map (fun id ->
           Printf.sprintf "get_%s()"
             (GenerateSchemaUtils.capitalizeFirstChar id))
    |> String.concat ", ")
    (printFields ~context:CtxInterface typ.fields)
    (Printf.sprintf "interface_%s_resolveType" typ.displayName)

let printInputObjectType (typ : gqlInputObjectType) =
  Printf.sprintf "{name: \"%s\", description: %s, fields: () => %s}"
    typ.displayName
    (descriptionAsString typ.description)
    (printInputObjectFields typ.fields)

let printUnionType (union : gqlUnion) =
  Printf.sprintf
    "{name: \"%s\", description: %s, types: () => [%s], resolveType: \
     GraphQLUnionType.makeResolveUnionTypeFn(%s)}"
    union.displayName
    (descriptionAsString union.description)
    (union.types
    |> List.sort (fun (m1 : gqlUnionMember) m2 ->
           String.compare m1.displayName m2.displayName)
    |> List.map (fun (member : gqlUnionMember) ->
           Printf.sprintf "get_%s()" member.displayName)
    |> String.concat ", ")
    (Printf.sprintf "union_%s_resolveType" union.displayName)

let getIntfAssets (typ : gqlInterface) ~processedSchema ~debug =
  let code = ref "/* @generated */\n\n@@warning(\"-27-34-37\")\n\n" in
  let addWithNewLine text = code := !code ^ text ^ "\n" in

  (* Interface assets *)
  let interfaceIdentifier = {id = typ.id; displayName = typ.displayName} in
  match Hashtbl.find_opt processedSchema.interfaceImplementedBy typ.id with
  | None -> ""
  | Some implementedBy ->
    addWithNewLine "module Resolver = {";
    addWithNewLine
      (printInterfaceResolverReturnType interfaceIdentifier ~implementedBy);
    addWithNewLine "}";
    addWithNewLine "";
    addWithNewLine "module ImplementedBy = {";
    addWithNewLine (printInterfaceImplementedByType ~implementedBy);
    addWithNewLine "";
    addWithNewLine (printInterfaceTypenameDecoder ~implementedBy);
    addWithNewLine "";
    addWithNewLine (printInterfaceTypenameToString ~implementedBy);
    addWithNewLine "}";
    addWithNewLine "";

    (* Special treatment of the Node interface. *)
    if typ.displayName = "Node" then
      addWithNewLine (printNodeInterfaceAssets implementedBy);
    !code |> formatCode ~debug

let mkIntfFileName intfId = Printf.sprintf "interface_%s.res" intfId

let mkIntfFilePath intfId ~outputFolder =
  Printf.sprintf "%s/%s" outputFolder (mkIntfFileName intfId)

let printInterfaceFiles (schemaState : schemaState) ~processedSchema ~debug
    ~outputFolder =
  schemaState.interfaces
  |> Hashtbl.iter (fun intfId intf ->
         let interfaceFileOutputLoc = mkIntfFilePath ~outputFolder intfId in
         writeIfHasChanges interfaceFileOutputLoc
           (getIntfAssets intf ~processedSchema ~debug))

let cleanInterfaceFiles (schemaState : schemaState) ~outputFolder =
  let validNames =
    Hashtbl.fold
      (fun intfId _ acc -> mkIntfFileName intfId :: acc)
      schemaState.interfaces []
  in
  let allGeneratedFiles = Array.to_list (Sys.readdir outputFolder) in
  let filesToRemove =
    allGeneratedFiles
    |> List.filter (fun fileName ->
           Filename.check_suffix fileName ".res"
           && String.starts_with
                (Filename.basename fileName)
                ~prefix:"interface_"
           && not (List.mem fileName validNames))
  in
  filesToRemove
  |> List.iter (fun fileName ->
         Sys.remove (Filename.concat outputFolder fileName))

exception Interface_not_found of string

let printSchemaJsFile schemaState processSchema =
  let code = ref "@@warning(\"-27-32\")\n\nopen ResGraph__GraphQLJs\n\n" in
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

  (* Add the input union unwrapper. TODO: Explain more
  *)
  addWithNewLine
    {|let inputUnionUnwrapper: ('src, array<string>) => 'return = %raw(`function inputUnionUnwrapper(src, inlineRecordTypenames) {
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
    
        return {
          TAG: tagName,
          _0: targetValue,
        };
      }
    
      return null;
    }
    `)|};

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

  (* Print all custom scalars. *)
  schemaState.scalars
  |> iterHashtblAlphabetically (fun _name (scalar : gqlScalar) ->
         addWithNewLine
           (Printf.sprintf "let scalar_%s = GraphQLScalar.make(%s)"
              scalar.displayName (printScalar scalar)));

  (* Print all enums. These won't have any other dependencies, so they can be printed as is. *)
  schemaState.enums
  |> iterHashtblAlphabetically (fun _name (enum : gqlEnum) ->
         addWithNewLine
           (Printf.sprintf
              "let enum_%s = GraphQLEnumType.make({name: \"%s\", description: \
               %s, values: {%s}->makeEnumValues})"
              enum.displayName enum.displayName
              (descriptionAsString enum.description)
              (enum.values
              |> List.map (fun (v : gqlEnumValue) ->
                     Printf.sprintf
                       "\"%s\": {GraphQLEnumType.value: \"%s\", description: \
                        %s, deprecationReason: %s}"
                       v.value v.value
                       (descriptionAsString v.description)
                       (undefinedOrValueAsString v.deprecationReason))
              |> String.concat ", ")));

  (* Print the interface type holders and getters *)
  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (typ : gqlInterface) ->
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
  |> iterHashtblAlphabetically (fun _ (typ : gqlObjectType) ->
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
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputObjectType) ->
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
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputObjectType) ->
         addWithNewLine (printInputObjectAssets typ));

  (* Print the union type holders and getters *)
  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
         addWithNewLine
           (Printf.sprintf
              "let union_%s: ref<GraphQLUnionType.t> = \
               Obj.magic({\"contents\": Js.null})"
              union.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => union_%s.contents"
              union.displayName union.displayName));

  (* Print the input union type holders and getters *)
  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (inputUnion : gqlInputUnionType) ->
         addWithNewLine
           (Printf.sprintf
              "let inputUnion_%s: ref<GraphQLInputObjectType.t> = \
               Obj.magic({\"contents\": Js.null})"
              inputUnion.displayName);
         addWithNewLine
           (Printf.sprintf "let get_%s = () => inputUnion_%s.contents"
              inputUnion.displayName inputUnion.displayName);
         addWithNewLine
           (Printf.sprintf "let inputUnion_%s_conversionInstructions = [];"
              inputUnion.displayName));

  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputUnionType) ->
         addWithNewLine (typ |> printInputUnionAssets ~state:schemaState));

  addWithNewLine "";

  (* Print support functions for union type resolution *)
  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
         addWithNewLine
           (Printf.sprintf
              "let union_%s_resolveType = (v: %s) => switch v {%s}\n"
              union.displayName
              (typeLocationToAccessor union.typeLocation)
              (union.types
              |> List.map (fun (member : gqlUnionMember) ->
                     Printf.sprintf " | %s(_) => \"%s\"" member.constructorName
                       member.displayName)
              |> String.concat "\n")));

  (* Print support functions for interface type resolution *)
  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (intf : gqlInterface) ->
         (* TODO: Flatten list properly when constructing *)
         let implementedBy =
           match
             Hashtbl.find_opt processSchema.interfaceImplementedBy intf.id
           with
           | Some i -> i
           | None -> raise (Interface_not_found ("Interface: " ^ intf.id))
         in
         addWithNewLine
           (Printf.sprintf
              "let interface_%s_resolveType = (v: Interface_%s.Resolver.t) => \
               switch v {%s}\n"
              intf.displayName intf.id
              (implementedBy
              |> List.map (fun (member : interfaceImplementedBy) ->
                     let displayName =
                       match member with
                       | ObjectType {displayName} | Interface {displayName} ->
                         displayName
                     in
                     Printf.sprintf " | %s(_) => \"%s\"" displayName displayName)
              |> String.concat "\n")));

  (* Now we can print all of the code that fills these in. *)
  schemaState.interfaces
  |> iterHashtblAlphabetically (fun _name (typ : gqlInterface) ->
         addWithNewLine
           (Printf.sprintf "i_%s.contents = GraphQLInterfaceType.make(%s)"
              typ.displayName
              (typ |> printInterfaceType)));

  schemaState.types
  |> iterHashtblAlphabetically (fun _name (typ : gqlObjectType) ->
         addWithNewLine
           (Printf.sprintf "t_%s.contents = GraphQLObjectType.make(%s)"
              typ.displayName (typ |> printObjectType)));

  schemaState.inputObjects
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputObjectType) ->
         addWithNewLine
           (Printf.sprintf "input_%s.contents = GraphQLInputObjectType.make(%s)"
              typ.displayName
              (typ |> printInputObjectType)));

  schemaState.inputUnions
  |> iterHashtblAlphabetically (fun _name (typ : gqlInputUnionType) ->
         addWithNewLine
           (Printf.sprintf
              "inputUnion_%s.contents = GraphQLInputObjectType.make(%s)"
              typ.displayName
              (typ
              |> inputUnionToInputObj ~state:schemaState
              |> printInputObjectType)));

  schemaState.unions
  |> iterHashtblAlphabetically (fun _name (union : gqlUnion) ->
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
