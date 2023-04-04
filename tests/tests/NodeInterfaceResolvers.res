@gql.field
let id = (user: User.user) => {
  NodeInterface.nodeInterfaceIdToString(~typename=User, ~id=user.id)
}
