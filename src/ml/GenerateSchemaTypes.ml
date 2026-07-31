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
  | InjectInterfaceTypename of {interfaceId: string; helperModule: string}
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

type authorizationMode =
  | AuthorizationOptional
  | AuthorizationRequired
  | AuthorizationBaseline

type authorizationConfig = {
  mode: authorizationMode;
  onForbidden: string option;
  manifestPath: string option;
  baselinePath: string option;
}

type authorizationFunctionReference = {
  path: string list;
  loc: Location.t;
  fileUri: Uri.t;
}

type publicAuthorization = {reason: string; loc: Location.t; fileUri: Uri.t}

type declaredAuthorization = {
  functions: authorizationFunctionReference list;
  public: publicAuthorization option;
}

type authorizationInjection = AuthorizationContext | AuthorizationInfo

type authorizationProvenance =
  | ObjectTypePolicy of string
  | InterfaceTypePolicy of string
  | InterfaceFieldPolicy of string
  | FieldPolicy of string

type authorizationFunction = {
  reference: authorizationFunctionReference;
  isAsync: bool;
  injections: authorizationInjection list;
  provenance: authorizationProvenance;
}

type resolverOutcome = {isAsync: bool}

type authorizationGapKind =
  | UncoveredField
  | MutationPreResolverPolicy
  | UnsupportedSubscription

type effectiveAuthorizationPlan = {
  functions: authorizationFunction list;
  public: publicAuthorization option;
  resolverOutcome: resolverOutcome option;
  synthetic: bool;
  baselineGap: authorizationGapKind option;
}

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

type gqlConstValue =
  | ConstNull
  | ConstInt of string
  | ConstFloat of string
  | ConstString of string
  | ConstBoolean of bool
  | ConstEnum of string
  | ConstList of gqlConstValue list
  | ConstObject of (string * gqlConstValue) list

type gqlDirectiveLocation =
  | LocationQuery
  | LocationMutation
  | LocationSubscription
  | LocationField
  | LocationFragmentDefinition
  | LocationFragmentSpread
  | LocationInlineFragment
  | LocationVariableDefinition
  | LocationSchema
  | LocationScalar
  | LocationObject
  | LocationFieldDefinition
  | LocationArgumentDefinition
  | LocationInterface
  | LocationUnion
  | LocationEnum
  | LocationEnumValue
  | LocationInputObject
  | LocationInputFieldDefinition

type gqlDirectiveTarget =
  | DirectiveSchema
  | DirectiveScalar of string
  | DirectiveObject of string
  | DirectiveFieldDefinition of {parentTypeName: string; fieldName: string}
  | DirectiveArgumentDefinition of {
      parentTypeName: string;
      fieldName: string;
      argumentName: string;
    }
  | DirectiveDirectiveArgumentDefinition of {
      directiveName: string;
      argumentName: string;
    }
  | DirectiveInterface of string
  | DirectiveUnion of string
  | DirectiveEnum of string
  | DirectiveEnumValue of {enumName: string; valueName: string}
  | DirectiveInputObject of string
  | DirectiveInputFieldDefinition of {
      inputObjectName: string;
      fieldName: string;
    }

type gqlDirectiveApplication = {
  name: string;
  arguments: (string * gqlConstValue) list;
  loc: Location.t;
  fileUri: Uri.t;
}

type gqlArg = {
  name: string;
  isOptionLabelled: bool;
      (* If the argument in ReScript is an optional label. *)
  typ: graphqlType;
  defaultValue: gqlConstValue option;
  description: string option;
  deprecationReason: string option;
  loc: Location.t;
  fileUri: Uri.t;
}

type gqlDirectiveArgument = {
  name: string;
  typ: graphqlType;
  description: string option;
  defaultValue: gqlConstValue option;
  deprecationReason: string option;
  loc: Location.t;
}

type gqlDirectiveDefinition = {
  name: string;
  description: string option;
  arguments: gqlDirectiveArgument list;
  locations: gqlDirectiveLocation list;
  repeatable: bool;
  typeLocation: typeLocationLoc;
}

type gqlInterfaceIdentifier = {id: string; displayName: string}

type explicitInterfaceImplementation = {
  interfaceName: string;
  loc: Location.t;
  fileUri: Uri.t;
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
  defaultValue: gqlConstValue option;
  deprecationReason: string option;
  description: string option;
  loc: Location.t;
  fileName: string;
  fileUri: Uri.t;
  onType: string option;
      (** The type this field is on, if that information is needed *)
  inheritedFromInterface: string option;
      (** The interface that supplied an inherited resolver field. *)
}

type syntheticTypeLocation = {fileUri: Uri.t; loc: Location.t}

type gqlObjectType = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocation option;
  syntheticTypeLocation: syntheticTypeLocation option;
  interfaces: string list;
  explicitInterfaces: explicitInterfaceImplementation list;
}

type gqlInterface = {
  id: string;
  displayName: string;
  fields: gqlField list;
  description: string option;
  typeLocation: typeLocationLoc;
  interfaces: string list;
  explicitInterfaces: explicitInterfaceImplementation list;
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
  contextTypePath: string list;
  rootFileUri: Uri.t;
  types: (string, gqlObjectType) Hashtbl.t;
  inputObjects: (string, gqlInputObjectType) Hashtbl.t;
  inputUnions: (string, gqlInputUnionType) Hashtbl.t;
  enums: (string, gqlEnum) Hashtbl.t;
  unions: (string, gqlUnion) Hashtbl.t;
  interfaces: (string, gqlInterface) Hashtbl.t;
  scalars: (string, gqlScalar) Hashtbl.t;
  directiveDefinitions: (string, gqlDirectiveDefinition) Hashtbl.t;
  appliedDirectives:
    (gqlDirectiveTarget, gqlDirectiveApplication list) Hashtbl.t;
  processedFiles: (string, bool) Hashtbl.t;
  authorizationConfig: authorizationConfig;
  authorizationDeclarations: (string, declaredAuthorization) Hashtbl.t;
  authorizationPlans: (string, effectiveAuthorizationPlan) Hashtbl.t;
  resolverOutcomes: (string, resolverOutcome) Hashtbl.t;
  authorizationExemptions: (string, unit) Hashtbl.t;
  mutable authorizationGaps: (string * authorizationGapKind) list;
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
  | Directive
