let fromArray: array<'value> => ResGraph.AsyncIterable.t<'value> = %raw(`values => ({
  async *[Symbol.asyncIterator]() {
    for (const value of values) yield value
  }
})`)

@gql.field
let asyncValues = (_: Query.query): ResGraph.AsyncIterable.t<string> =>
  fromArray(["first", "second"])
