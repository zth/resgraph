/**An object with an ID*/
@gql.interface
type node = {
  /** The id of the object.*/
  @gql.field
  id: ResGraph.id,
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
