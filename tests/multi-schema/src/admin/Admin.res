@gql.type
type query

@gql.field
let adminValue = (_: query, ~ctx: AdminContext.context) => ctx.actorId

@gql.field
let sharedItem = (_: query): Shared.sharedItem => {id: "admin", label: "Shared"}
