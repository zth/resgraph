type reason = Denied

let good = (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let missingArgs = (_: Query.query): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let unavailableArg = (_: Query.query, ~args: {"missing": string}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => {
  let _ = args["missing"]
  ResGraph.Authorization.Allowed()
}

let badReturn = (_: Query.query, ~args: {.}): unit => ()

let extraLabel = (_: Query.query, ~args: {.}, ~tenant: string): ResGraph.Authorization.outcome<
  unit,
  reason,
> => {
  let _ = tenant
  ResGraph.Authorization.Allowed()
}

let wrongSource = (_: Mutation.mutation, ~args: {.}): ResGraph.Authorization.outcome<
  unit,
  reason,
> => ResGraph.Authorization.Allowed()

let badArgsObject = (_: Query.query, ~args: string): ResGraph.Authorization.outcome<
  unit,
  reason,
> => {
  let _ = args
  ResGraph.Authorization.Allowed()
}

let badContext = (_: Query.query, ~args: {.}, ~ctx: string): ResGraph.Authorization.outcome<
  unit,
  reason,
> => {
  let _ = ctx
  ResGraph.Authorization.Allowed()
}

let nonUnitOutcome = (_: Query.query, ~args: {.}): ResGraph.Authorization.outcome<
  string,
  reason,
> => ResGraph.Authorization.Allowed("wrong")

let canSelect = (_: Query.query, ~args: {.}) => ResGraph.Authorization.Allowed()
