open ResGraphSchemaAssets

let typeMap: node_typeMap<int> = {
  group: 1,
  user: 2,
}

let nodeTypeMap = NodeInterfaceTypeMap.make(typeMap, ~valueToString=Int.toString)

let decodeNodeInterfaceId = id => {
  open ResGraphSchemaAssets

  switch id->ResGraph.idToString->String.split(":")->List.fromArray {
  | list{typeValue, id, ...params} =>
    switch nodeTypeMap->NodeInterfaceTypeMap.getTypeByStringifiedValue(typeValue) {
    | None => None
    | Some(typ) => Some(typ, id, params->List.toArray)
    }
  | _ => None
  }
}

/** Sketched out examples of type safe id producing/decoding functions for the
    Node interface. Could be base64 encoded etc. */
let nodeInterfaceIdToString = (~typename: node_implementedBy, ~id, ~extra=[]) => {
  let value = nodeTypeMap->NodeInterfaceTypeMap.getStringifiedValueByType(typename)
  let nodeId = `${value}:${id}`

  let nodeId = if extra->Array.length > 0 {
    `${nodeId}:${extra->Array.joinWith(":")}`
  } else {
    nodeId
  }

  nodeId->ResGraph.Utils.Base64.encode->ResGraph.id
}

/** Fetches an object given its ID.*/
@gql.field
let node = async (_: Query.query, ~id, ~ctx: ResGraphContext.context): option<node_resolver> => {
  switch decodeNodeInterfaceId(id) {
  | None => None
  | Some(typename, id, _extraParams) =>
    switch typename {
    | User =>
      switch await ctx.dataLoaders.user.byId->DataLoader.load(id) {
      | Error(_) => None
      | Ok(user) => Some(User(user))
      }
    | Group =>
      Some(
        Group({
          Schema.name: "TestGroup",
          memberIds: [],
          id,
          createdAt: None,
          modifiedAt: None,
        }),
      )
    }
  }
}

/** Fetches objects given their IDs. */
@gql.field
let nodes = (query: Query.query, ~ids, ~ctx: ResGraphContext.context) => {
  ids->Array.map(id => node(query, ~id, ~ctx))
}

/** The id of the object.*/
@gql.field
let id = (node: NodeInterface.node, ~typename: node_implementedBy) => {
  nodeInterfaceIdToString(~typename, ~id=node.id)
}
