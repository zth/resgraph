open Interface_node

let typeMap: typeMap<int> = {
  group: 1,
  user: 2,
}

let nodeTypeMap = TypeMap.make(typeMap, ~valueToString={v => Int.toString(v)})

let decodeNodeInterfaceId = id => {
  switch id->ResGraph.idToString->String.split(":")->List.fromArray {
  | list{typeValue, id, ...params} =>
    switch nodeTypeMap->TypeMap.getTypeByStringifiedValue(typeValue) {
    | None => None
    | Some(typ) => Some(typ, id, params->List.toArray)
    }
  | _ => None
  }
}

/** Sketched out examples of type safe id producing/decoding functions for the
    Node interface. Could be base64 encoded etc. */
let nodeInterfaceIdToString = (~typename: ImplementedBy.t, ~id, ~extra=[]) => {
  let value = nodeTypeMap->TypeMap.getStringifiedValueByType(typename)
  let nodeId = `${value}:${id}`

  let nodeId = if extra->Array.length > 0 {
    `${nodeId}:${extra->Array.join(":")}`
  } else {
    nodeId
  }

  nodeId->ResGraph.Utils.Base64.encode->ResGraph.id
}

/** Fetches an object given its ID.*/
@gql.field
let node = async (_: Query.query, ~id, ~ctx: ResGraphContext.context): option<Resolver.t> => {
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
let id = (node: NodeInterface.node, ~typename: ImplementedBy.t) => {
  nodeInterfaceIdToString(~typename, ~id=node.id)
}
