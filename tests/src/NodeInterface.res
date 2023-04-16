/**An object with an ID*/
@gql.interface
type node = {
  /** The id of the object.*/
  @gql.field
  id: ResGraph.id,
}
