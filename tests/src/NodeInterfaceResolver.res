@gql.field
let node = async (_: Query.query, ~id, ~ctx: ResGraphContext.context): option<
  ResGraphSchemaAssets.node_resolver,
> => {
  let (typename, params) = NodeInterface.decodeNodeInterfaceId(id)

  switch typename {
  | None => None
  | Some(User) =>
    switch params {
    | [userId] =>
      switch await ctx.userById(~userId) {
      | None => None
      | Some(user) => Some(User(user))
      }
    | _ => None
    }
  | Some(Group) =>
    switch params {
    | [groupId] => Some(Group({Schema.name: "TestGroup", memberIds: [], id: groupId->ResGraph.id}))
    | _ => None
    }
  }
}
