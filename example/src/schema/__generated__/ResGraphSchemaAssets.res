/* @generated */

@@warning("-27-34-37")

@gql.interfaceResolver("hasName") type hasName_resolver = User(GraphQLSchema.user)

type hasName_implementedBy = User

let decodeImplementedByInterface_hasName = (str: string) =>
  switch str {
  | "User" => Some(User)
  | _ => None
  }

external hasName_typenameToString: hasName_implementedBy => string = "%identity"
