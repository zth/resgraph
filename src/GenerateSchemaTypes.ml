type scalar = Int | Float | String | Boolean | ID

type graphqlType =
  | List of graphqlType
  | Nullable of graphqlType
  | RescriptNullable of graphqlType
  | Scalar of scalar
  | InjectContext
  | GraphQLObjectType of {id: string; displayName: string}
  | GraphQLInputObject of {id: string; displayName: string}
  | GraphQLEnum of {id: string; displayName: string}
  | GraphQLUnion of {id: string; displayName: string}
  | GraphQLInterface of {id: string; displayName: string}

type fieldResolverStyle =
  | Resolver of {moduleName: string; fnName: string; pathToFn: string list}
  | Property of string

type typeLocation = {
  fileName: string;
  fileUri: Uri.t;
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

type gqlInterfaceIdentifier = {id: string; displayName: string}

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
  interfaces: gqlInterfaceIdentifier list;
}

type gqlInterface = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocation;
  interfaces: gqlInterfaceIdentifier list;
}

type gqlInputObjectType = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocation;
}

type schemaState = {
  types: (string, gqlObjectType) Hashtbl.t;
  inputObjects: (string, gqlInputObjectType) Hashtbl.t;
  enums: (string, gqlEnum) Hashtbl.t;
  unions: (string, gqlUnion) Hashtbl.t;
  interfaces: (string, gqlInterface) Hashtbl.t;
  processedFiles: (string, bool) Hashtbl.t;
  mutable query: gqlObjectType option;
  mutable subscription: gqlObjectType option;
  mutable mutation: gqlObjectType option;
  mutable diagnostics: diagnostic list;
}

type interfaceImplementedBy =
  | ObjectType of gqlObjectType
  | Interface of gqlInterface

(* This holds all of the things we need to wait til after all processing has
   completed to calculate.*)
type processedSchema = {
  interfaceImplementedBy: (string, interfaceImplementedBy list) Hashtbl.t;
}

type gqlAttributes =
  | ObjectType of {interfaces: Longident.t Location.loc list}
  | Interface of {interfaces: Longident.t Location.loc list}
  | InterfaceResolver of {interfaceId: string}  (** This is internal *)
  | InputObject
  | Field
  | Enum
  | Union
