exception Fail of string

type scalar = Int | Float | String | Boolean | ID

type graphqlType =
  | List of graphqlType
  | Nullable of graphqlType
  | RescriptNullable of graphqlType
  | Scalar of scalar
  | (* TODO: Get rid of *) Named of {path: Path.t; env: SharedTypes.QueryEnv.t}
  | InjectContext
  | GraphQLObjectType of {id: string; displayName: string}
  | GraphQLInputObject of {id: string; displayName: string}
  | GraphQLEnum of {id: string; displayName: string}
  | GraphQLUnion of {id: string; displayName: string}

type fieldResolverStyle =
  | Resolver of {moduleName: string; fnName: string; pathToFn: string list}
  | Property of string

type typeLocation = {
  fileName: string;
  modulePath: string list;
  typeName: string;
  loc: Location.t;
}

type diagnostic = {loc: Location.t; fileUri: Uri.t; message: string}

type gqlArg = {
  name: string;
  isOptionLabelled: bool;
      (* If the argument in ReScript is an optional label. *)
  typ: graphqlType; (* TODO: Default value. *)
}

type gqlEnumValue = {
  value: string;
  description: string option;
  deprecationReason: string option;
  loc: Location.t;
}

type gqlEnum = {
  id: string;
  displayName: string;
  values: gqlEnumValue list;
  description: string option;
  loc: Location.t;
}

type gqlUnionMember = {
  objectTypeId: string;
  displayName: string;
  loc: Location.t;
}

type gqlUnion = {
  id: string;
  displayName: string;
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
  typ: graphqlType;
  args: gqlArg list;
  deprecationReason: string option;
  description: string option;
  loc: Location.t;
}

type gqlObjectType = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocation;
}

type gqlInputObjectType = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocation;
}

type state = {
  types: (string, gqlObjectType) Hashtbl.t;
  inputObjects: (string, gqlInputObjectType) Hashtbl.t;
  enums: (string, gqlEnum) Hashtbl.t;
  unions: (string, gqlUnion) Hashtbl.t;
  mutable query: gqlObjectType option;
  mutable subscription: gqlObjectType option;
  mutable mutation: gqlObjectType option;
  mutable diagnostics: diagnostic list;
}

type gqlAttributes = ObjectType | InputObject | Field | Enum | Union

let pathIdentToList (p : Path.t) =
  let rec pathIdentToListInner ?(acc = []) (p : Path.t) =
    match p with
    | Pident {name} -> name :: acc
    | Pdot (nextPath, id, _) -> [id] @ pathIdentToListInner ~acc nextPath
    | Papply _ -> acc
  in
  let lst = pathIdentToListInner p in
  lst |> List.rev