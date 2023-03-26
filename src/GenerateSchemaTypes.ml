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

type fieldResolverStyle =
  | Resolver of {moduleName: string; fnName: string; pathToFn: string list}
  | Property of string

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

let argIsOptional arg =
  match arg.typ with
  | Nullable _ -> true
  | _ -> false

type gqlField = {
  name: string;
  resolverStyle: fieldResolverStyle;
  typ: returnType;
  args: gqlArg list;
}

type typ = {name: string; fields: gqlField list}

type state = {
  types: (string, typ) Hashtbl.t;
  enums: (string, gqlEnum) Hashtbl.t;
  query: typ option;
}

type gqlAttributes = ObjectType | Field | Enum

let rec returnTypeToString returnType =
  match returnType with
  | Nullable inner -> "Nullable(" ^ returnTypeToString inner ^ ")"
  | Named {path} -> "Named(" ^ SharedTypes.pathIdentToString path ^ ")"
  | GraphQLObjectType {name} -> "ObjectType(" ^ name ^ ")"
  | GraphQLEnum {name} -> "Enum(" ^ name ^ ")"
  | Scalar scalar -> (
    match scalar with
    | Int -> "Int"
    | Float -> "Float"
    | String -> "String"
    | Boolean -> "Boolean"
    | ID -> "ID")

let pathIdentToList (p : Path.t) =
  let rec pathIdentToListInner ?(acc = []) (p : Path.t) =
    match p with
    | Pident {name} -> name :: acc
    | Pdot (nextPath, id, _) -> [id] @ pathIdentToListInner ~acc nextPath
    | Papply _ -> acc
  in
  let lst = pathIdentToListInner p in
  lst |> List.rev