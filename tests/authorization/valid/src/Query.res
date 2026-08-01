@gql.type
type query

type inferredResult = [#Success({"value": string}) | #Failure({"message": string})]

@gql.public({reason: "Health check contains no private data"}) @gql.field
let health = (_: query) => "ok"

@gql.authorize(Security.canFindUser) @gql.field
let user = (_: query, ~id: string): User.user => {id, secret: "secret"}

@gql.field
let outcome = (_: query): ResGraph.Authorization.outcome<
  string,
  string,
> => ResGraph.Authorization.Allowed("allowed")

@gql.authorize(Security.canLoadAsync) @gql.field
let asyncValue = (_: query): string => "async"

@gql.authorize(Security.canLoadDevice) @gql.field
let device = (_: query): Device.device => {name: "device", serial: "123"}

@gql.authorize(Security.first) @gql.authorize(Security.Alias.second) @gql.field
let ordered = (_: query): string => "ordered"

@gql.authorize(Security.first) @gql.field
let inferred = (_: query): inferredResult => #Success({"value": "visible"})

@gql.field
let asyncOutcome = async (_: query): ResGraph.Authorization.outcome<
  string,
  string,
> => ResGraph.Authorization.Allowed("async allowed")

@gql.authorize(Security.canLoadPublicDevice) @gql.field
let publicDevice = (_: query): PublicDevice.publicDevice => {label: "public"}

@gql.authorize(Security.canLoadOutcomeDevice) @gql.field
let outcomeDevice = (_: query): OutcomeDevice.outcomeDevice => {id: "outcome"}

@gql.union
type declaredResult = Success({value: string}) | Failure({message: string})

@gql.authorize(Security.first) @gql.field
let declared = (_: query): declaredResult => Success({value: "visible"})

@gql.authorize(
  (Security.canLoadSelection, {scope: Fields})
)
@gql.field
let selection = (_: query): SelectionCoverage.selectionConnection => {
  edges: [{cursor: "cursor", node: {value: "selected"}}],
}
