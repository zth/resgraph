open GenerateSchemaTypes

let extractAttributes (attributes : Parsetree.attributes) =
  attributes
  |> List.filter_map (fun ((name, _payload) : Parsetree.attribute) ->
         if Utils.startsWith name.txt "gql." then
           match String.split_on_char '.' name.txt with
           | ["gql"; "type"] -> Some ObjectType
           | ["gql"; "field"] -> Some Field
           | ["gql"; "enum"] -> Some Enum
           | _ -> (* TODO: Warn about invalid gql annotation*) None
         else None)

(* TODO: Refactor to only allow one gql attribute. Leverage payloads instead *)

let extractGqlAttribute (attributes : Parsetree.attributes) =
  match extractAttributes attributes with
  | attr :: _ -> Some attr
  | _ -> None

let getFieldAttribute gqlAttributes =
  gqlAttributes
  |> List.find_map (fun attr ->
         match attr with
         | Field -> Some attr
         | _ -> None)

let getFieldAttributeFromRawAttributes attributes =
  attributes |> extractAttributes |> getFieldAttribute

let formatCode code =
  let {Res_driver.parsetree = structure; comments; diagnostics} =
    Res_driver.parseImplementationFromSource ~forPrinter:true ~source:code
      ~displayFilename:"Schema.res"
  in
  let printed =
    Res_printer.printImplementation ~width:!Res_cli.ResClflags.width ~comments
      structure
  in
  if List.length diagnostics > 0 then
    "\n\n== SYNTAX ERRORS ==\n"
    ^ (Diagnostics.get_diagnostics diagnostics |> String.concat "\n\n")
    ^ "\n\n== CODE ==\n" ^ printed
  else printed

let capitalizeFirstChar s =
  if String.length s = 0 then s
  else String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s

let returnTypeFromItem (item : SharedTypes.Type.t) =
  let gqlAttribute = extractGqlAttribute item.attributes in
  match (gqlAttribute, item) with
  | Some ObjectType, {kind = Record _; name} ->
    Some (GraphQLObjectType {name = capitalizeFirstChar name})
  | Some Enum, {kind = Variant _; name} ->
    Some (GraphQLEnum {name = capitalizeFirstChar name})
  | _ -> None

let noticeObjectType typeName ~state =
  if Hashtbl.mem state.types typeName then
    Printf.printf "already seen %s\n" typeName
  else (
    Printf.printf "noticing %s\n" typeName;
    Hashtbl.add state.types typeName {name = typeName; fields = []})

let addEnum enumName ~(enum : gqlEnum) ~state =
  Printf.printf "Adding enum %s\n" enumName;
  Hashtbl.replace state.enums enumName enum

let addFieldToObjectType typeName ~field ~state =
  let typ =
    match Hashtbl.find_opt state.types typeName with
    | None -> {name = typeName; fields = [field]}
    | Some typ -> {typ with fields = field :: typ.fields}
  in
  Hashtbl.replace state.types typeName typ

let getOrRaise opt msg =
  match opt with
  | None -> raise (Failure msg)
  | Some v -> v
