open Interface_labelled

@gql.type
type labelledWrapper = {
  @gql.field nested: Resolver.t,
}

@gql.field
let goodLabelled = (_: Query.query): Resolver.t =>
  LabelledBeta({
    name: "Beta",
    count: 2,
  })

@gql.field
let badLabelled = (_: Query.query): Resolver.t =>
  Obj.magic(
    ({
      name: "Alpha",
      extra: "untagged alpha",
    }: AppLabelledTypes.labelledAlpha),
  )

@gql.field
let labelledWrapper = (_: Query.query): labelledWrapper => {
  nested: LabelledAlpha({
    name: "Alpha",
    extra: "nested alpha",
  }),
}

@gql.field
let brokenLabelledWrapper = (_: Query.query): labelledWrapper => {
  nested: Obj.magic(
    ({
      name: "Alpha",
      extra: "broken nested alpha",
    }: AppLabelledTypes.labelledAlpha),
  ),
}
