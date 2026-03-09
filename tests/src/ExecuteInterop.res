let variables: ResGraph.Execute.variables = dict{
  "limit": JSON.Number(10.),
  "includeInactive": JSON.Boolean(false),
}

let variablesJson: JSON.t = variables->ResGraph.Execute.variablesToJson

let roundTrippedVariables =
  variablesJson
  ->ResGraph.Execute.variablesFromJson
  ->Option.getOrThrow(~message="Expected GraphQL variables JSON object")

let responseJson: JSON.t =
  {
    data: JSON.Object(dict{
      "ok": JSON.Boolean(true),
      "limit": JSON.Number(10.),
    }),
    errors: [
      JSON.Object(dict{
        "message": JSON.String("Example error"),
      }),
    ],
  }->ResGraph.Execute.executionResultToJson

let _variablesCount = roundTrippedVariables->Dict.keysToArray->Array.length
let _responseIsObject = switch responseJson {
| JSON.Object(_) => true
| _ => false
}
