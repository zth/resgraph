@gql.inputObject
type scalarMetadataInput = {label: string}

@gql.enum
type scalarTier = Premium

@gql.directive({locations: ["SCALAR"]})
type scalarMetadata = {config: scalarMetadataInput, tier: scalarTier}

/** Custom scalar with specifiedByUrl coverage. */
@specifiedBy("https://example.com/specifiedBy/uuid")
@gql.annotate({
  name: "scalarMetadata",
  args: {config: {label: "deferred"}, tier: Premium},
})
@gql.annotate({name: "tag", args: {name: "scalar"}})
@gql.scalar
type uuid = string
// ^hov

@gql.type
type scalarHolder = {
  @gql.field id: uuid,
}

@gql.field
let getScalarHolder = (_: Query.query): scalarHolder => {id: "abc"}
// ^hov
