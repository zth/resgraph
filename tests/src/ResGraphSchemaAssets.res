/* @generated */

@@warning("-27-34-37")

@gql.interfaceResolver("node") type node_resolver = Group(Schema.group) | User(User.user)

type node_implementedBy = Group | User

let decodeImplementedByInterface_node = (str: string) =>
  switch str {
  | "Group" => Some(Group)
  | "User" => Some(User)
  | _ => None
  }

external node_typenameToString: node_implementedBy => string = "%identity"

@gql.interfaceResolver("hasName") type hasName_resolver = Group(Schema.group) | User(User.user)

type hasName_implementedBy = Group | User

let decodeImplementedByInterface_hasName = (str: string) =>
  switch str {
  | "Group" => Some(Group)
  | "User" => Some(User)
  | _ => None
  }

external hasName_typenameToString: hasName_implementedBy => string = "%identity"
