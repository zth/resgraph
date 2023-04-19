let decodeNodeInterfaceId = id => {
  let id = id->ResGraph.idToString
  switch id->String.split(":")->List.fromArray {
  | list{id, ...params} => (
      id->ResGraphSchemaAssets.decodeImplementedByInterface_node,
      params->List.toArray,
    )
  | list{} => (None, [])
  }
}

/** Sketched out examples of type safe id producing/decoding functions for the
    Node interface. Could be base64 encoded etc. */
let nodeInterfaceIdToString = (
  ~typename: ResGraphSchemaAssets.node_implementedBy,
  ~id,
  ~extra=[],
) => {
  let rawId =
    `${typename->ResGraphSchemaAssets.node_typenameToString}:${id}` ++ if extra->Array.length > 0 {
      `:${extra->Array.joinWith(":")}`
    } else {
      ""
    }

  rawId->ResGraph.id
}

/** Fetches an object given its ID.*/
@gql.field
let node = async (_: Query.query, ~id, ~ctx: ResGraphContext.context): option<
  ResGraphSchemaAssets.node_resolver,
> => {
  let (typename, params) = decodeNodeInterfaceId(id)

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
    | [groupId] =>
      Some(
        Group({Schema.name: "TestGroup", memberIds: [], id: groupId->ResGraph.id, createdAt: None}),
      )
    | _ => None
    }
  }
}

/** Fetches objects given their IDs. */
@gql.field
let nodes = (query: Query.query, ~ids, ~ctx: ResGraphContext.context) => {
  ids->Array.map(id => node(query, ~id, ~ctx))
}

@gql.field
let id = (user: User.user) => {
  nodeInterfaceIdToString(~typename=User, ~id=user.id->ResGraph.idToString)
}
