@gql.field
let signatureMetadata = (
  _: Query.query,
  @gql.description("Metadata loaded from the implementation source.")
  ~value: string,
) => value
