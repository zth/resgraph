/** Extra coverage for ReScript 12 CMT/attribute changes. */
@gql.type
type res12Record = {
  /** Doc should survive on fields. */
  @gql.field
  withDoc: string,
  /** Deprecated attribute should survive too. */
  @deprecated("old field")
  @gql.field
  oldField: int,
}

@gql.inputUnion
type res12Input =
  /** Inline record doc is preserved. */
  | Inline({
      /** Field doc on inline record. */
      payload: string,
    })
  | Empty
