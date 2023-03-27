(* TODO: Granular exceptions. Loc-linked errors. All errors at the same time,
   not just the first encountered. *)
exception Fail of string

type scalar = Int | Float | String | Boolean | ID

(* TODO: List *)
type returnType =
  | Nullable of returnType
  | Scalar of scalar
  | Named of {path: Path.t; env: SharedTypes.QueryEnv.t}
  | GraphQLObjectType of {name: string}
  | GraphQLEnum of {name: string}
  | GraphQLUnion of {name: string}

type fieldResolverStyle =
  | Resolver of {moduleName: string; fnName: string; pathToFn: string list}
  | Property of string

type typeLocation = {
  fileName: string;
  modulePath: string list;
  typeName: string;
}

type gqlArg = {name: string; typ: returnType (* TODO: Default value. *)}

type gqlEnumValue = {
  value: string;
  description: string option;
  deprecationReason: string option;
}

type gqlEnum = {
  name: string;
  values: gqlEnumValue list;
  description: string option;
}

type gqlUnionMember = {objectTypeName: string}

type gqlUnion = {
  name: string;
  description: string option;
  types: gqlUnionMember list;
  typeLocation: typeLocation;
}

let argIsOptional arg =
  match arg.typ with
  | Nullable _ -> true
  | _ -> false

type gqlField = {
  name: string;
  resolverStyle: fieldResolverStyle;
  typ: returnType;
  args: gqlArg list;
  deprecationReason: string option;
  description: string option;
}

type gqlObjectType = {
  name: string;
  fields: gqlField list;
  description: string option;
}

type state = {
  types: (string, gqlObjectType) Hashtbl.t;
  enums: (string, gqlEnum) Hashtbl.t;
  unions: (string, gqlUnion) Hashtbl.t;
  query: gqlObjectType option;
}

type gqlAttributes = ObjectType | Field | Enum | Union

let pathIdentToList (p : Path.t) =
  let rec pathIdentToListInner ?(acc = []) (p : Path.t) =
    match p with
    | Pident {name} -> name :: acc
    | Pdot (nextPath, id, _) -> [id] @ pathIdentToListInner ~acc nextPath
    | Papply _ -> acc
  in
  let lst = pathIdentToListInner p in
  lst |> List.rev