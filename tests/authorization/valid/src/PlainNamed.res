@gql.interface
type plainNamed = {
  @gql.public({reason: "Identifiers are public on the plain interface"}) @gql.field
  id: string,
}

@gql.public({reason: "Plain computed values are public"}) @gql.field
let computed = (value: plainNamed): string => value.id
