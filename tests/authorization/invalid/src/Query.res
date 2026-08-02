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

@gql.public({reason: "a b"}) @gql.field
let shortPublicReason = (_: query): string => "too short"

@gql.authorize(Security.wrongSource) @gql.field
let wrongSource = (_: query): string => "wrong source"

@gql.authorize(Security.badArgsObject) @gql.field
let badArgsObject = (_: query): string => "bad args"

@gql.authorize(Security.badContext) @gql.field
let badContext = (_: query): string => "bad context"

@gql.authorize(Security.nonUnitOutcome) @gql.field
let nonUnitOutcome = (_: query): string => "bad outcome"

@gql.type
@gql.authorize.byAncestor({reason: "Shared selection fields rely on their protected entry path"})
type sharedSelection = {
  @gql.field
  value: string,
}

@gql.authorize(Security.canSelect) @gql.field
let protectedSelection = (_: query): sharedSelection => {value: "protected"}

@gql.public({reason: "Intentionally exercises an unprotected alternate path"}) @gql.field
let publicSelection = (_: query): sharedSelection => {value: "public"}

@gql.authorize.byAncestor({reason: "Root field incorrectly claims an ancestor policy"}) @gql.field
let noAncestor = (_: query): string => "no ancestor"

@gql.authorize.byAncestor({reason: "Conflicts with a direct field policy"})
@gql.authorize(Security.good)
@gql.field
let ancestorConflict = (_: query): string => "conflict"

@gql.authorize.byAncestor({reason: "a b"}) @gql.field
let shortAncestorReason = (_: query): string => "short"

@gql.authorize.byAncestor({reason: "First review"})
@gql.authorize.byAncestor({reason: "Second review"})
@gql.field
let duplicateAncestor = (_: query): string => "duplicate"
