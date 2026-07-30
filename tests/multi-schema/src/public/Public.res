@gql.type
type query

@gql.field
let publicValue = (_: query, ~ctx: PublicContext.context) => ctx.requestId

@gql.field
let sharedItem = (_: query): Shared.sharedItem => {id: "public", label: "Shared"}
