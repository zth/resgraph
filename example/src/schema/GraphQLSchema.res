@gql.type
type query = {}

@gql.type({interfaces: [HasNameInterface.hasName]})
type user = {@gql.field name: string, @gql.field age: int}

@gql.field
let me = (_: query, ~ctx: ResGraphContext.context): option<user> => {
  switch ctx.currentUserId {
  | None => None
  | Some(currentUserId) => await ctx.dataLoaders.loadUserById(currentUserId)
  }
}
