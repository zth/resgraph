type scalar = Int | Float | String | Boolean | ID

type graphqlType =
  | List of graphqlType
  | Nullable of graphqlType
  | RescriptNullable of graphqlType
  | Scalar of scalar
  | EmptyPayload
      (** Used to represent empty payloads, like constructor-less unions. *)
  | InjectContext
  | InjectInfo
  | InjectInterfaceTypename of string  (** ID of interface *)
  | GraphQLObjectType of {id: string; displayName: string}
  | GraphQLInputObject of {id: string; displayName: string}
  | GraphQLInputUnion of {
      id: string;
      displayName: string;
      inlineRecords: string list;
      emptyPayloads: string list;
    }
  | GraphQLEnum of {id: string; displayName: string}
  | GraphQLUnion of {id: string; displayName: string}
  | GraphQLInterface of {id: string; displayName: string}
  | GraphQLScalar of {id: string; displayName: string}

type fieldResolverStyle =
  | Resolver of {moduleName: string; fnName: string; pathToFn: string list}
  | Property of string

type typeLocationLoc = {
  fileName: string;
  fileUri: Uri.t;
  modulePath: string list;
  typeName: string;
  loc: Location.t;
}

type typeLocation =
  | Synthetic of {fileName: string; fileUri: Uri.t; modulePath: string list}
  | Concrete of typeLocationLoc

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
  typeLocation: typeLocation;
}

type gqlUnionMember = {
  objectTypeId: string;
  displayName: string;
      (** TODO: Rename this to something more descriptive... *)
  description: string option;
  constructorName: string;
  loc: Location.t;
}

type gqlUnionTypeSource = Variant | Polyvariant

type gqlUnion = {
  typeSource: gqlUnionTypeSource;
  id: string;
  displayName: string;
  description: string option;
  types: gqlUnionMember list;
  typeLocation: typeLocation;
}

type gqlScalar = {
  id: string;
  displayName: string;
  description: string option;
  typeLocation: typeLocationLoc;
  specifiedByUrl: string option;
  encoderDecoderLoc: typeLocationLoc option;
}

(* TODO: Can this be thinned out for some cases? Should be split up. *)
type gqlField = {
  name: string;
  resolverStyle: fieldResolverStyle;
  typ: graphqlType;
  args: gqlArg list;
  deprecationReason: string option;
  description: string option;
  loc: Location.t;
  fileName: string;
  fileUri: Uri.t;
  onType: string option;
      (** The type this field is on, if that information is needed *)
}

type typeCreatorLocation = {env: SharedTypes.QueryEnv.t; loc: Location.t}
(** The location of the type creator. *)

type syntheticTypeLocation = {fileUri: Uri.t; loc: Location.t}

type gqlObjectType = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocation option;
  syntheticTypeLocation: syntheticTypeLocation option;
  (* TODO: Can be removed? *)
  typeCreatorLocation: typeCreatorLocation option;
      (** If this type is synthetic, this will hold the location of the type creator that created the type. *)
  interfaces: string list;
}

type gqlInterface = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocationLoc;
  interfaces: string list;
}

type gqlInputObjectType = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocationLoc option;
  syntheticTypeLocation: syntheticTypeLocation option;
}

type gqlInputUnionMember = {
  fieldName: string;
  typ: graphqlType;
  description: string option;
  constructorName: string;
  loc: Location.t;
}

type gqlInputUnionType = {
  id: string;
  displayName: string;
  members: gqlInputUnionMember list;
  description: string option;
  typeLocation: typeLocationLoc;
}

type schemaState = {
  types: (string, gqlObjectType) Hashtbl.t;
  inputObjects: (string, gqlInputObjectType) Hashtbl.t;
  inputUnions: (string, gqlInputUnionType) Hashtbl.t;
  enums: (string, gqlEnum) Hashtbl.t;
  unions: (string, gqlUnion) Hashtbl.t;
  interfaces: (string, gqlInterface) Hashtbl.t;
  scalars: (string, gqlScalar) Hashtbl.t;
  processedFiles: (string, bool) Hashtbl.t;
  mutable query: gqlObjectType option;
  mutable subscription: gqlObjectType option;
  mutable mutation: gqlObjectType option;
  mutable diagnostics: (string * diagnostic) list;
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
  | ObjectType
  | Interface
  | InterfaceResolver of {interfaceId: string}  (** This is internal *)
  | InputObject
  | InputUnion
  | Field
  | Enum
  | Union
  | Scalar
