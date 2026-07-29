@gql.public({reason: "Types cannot be public"}) @gql.type
type query

@gql.field
let uncovered = (_: query): string => "uncovered"

@gql.authorize(Missing.policy) @gql.field
let missingPolicy = (_: query): string => "missing"

@gql.authorize(Security.missingArgs) @gql.field
let missingArgs = (_: query): string => "missing args"

@gql.authorize(Security.unavailableArg) @gql.field
let unavailableArg = (_: query): string => "bad arg"

@gql.authorize(Security.badReturn) @gql.field
let badReturn = (_: query): string => "bad return"

@gql.authorize(Security.extraLabel) @gql.field
let extraLabel = (_: query): string => "extra label"

@gql.public({reason: "Conflicting disposition"}) @gql.authorize(Security.good) @gql.field
let publicConflict = (_: query): string => "conflict"

@gql.public({reason: "First"}) @gql.public({reason: "Second"}) @gql.field
let duplicatePublic = (_: query): string => "duplicate"

@gql.authorize(Security.wrongSource) @gql.field
let wrongSource = (_: query): string => "wrong source"

@gql.authorize(Security.badArgsObject) @gql.field
let badArgsObject = (_: query): string => "bad args"

@gql.authorize(Security.badContext) @gql.field
let badContext = (_: query): string => "bad context"

@gql.authorize(Security.nonUnitOutcome) @gql.field
let nonUnitOutcome = (_: query): string => "bad outcome"
