// Only records are allowed for types
@gql.type type someAbstractType

// Only valid fields can be exposed as GraphQL fields directly
@gql.type type user = {@gql.field someDict: Js.Dict.t<string>}

// Enums must be variants
@gql.enum type wrongEnum

// Enums cannot have payloads
@gql.enum type myEnum = Online | Offline | User(user)

type groupWithNoAnnotation = {@gql.field name: string}

// Unions must be variants
@gql.enum type wrongUnion

// Unions must have payloads, and they must be GraphQL object types
@gql.union type someUnion = User(user) | Group(groupWithNoAnnotation)

// Input objects must be records
@gql.inputObject type wrongInputObject

// Resolvers must be functions
@gql.field let wrongResolver = ()

// Object fields must not be promises
@gql.type type group = {@gql.field name: promise<string>}
@gql.inputObject type group2 = {name: promise<string>}

// ^ gen -- disabled for now
