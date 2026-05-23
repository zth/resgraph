@gql.type
type user = {
  @gql.field id: string,
  @gql.field name: string,
  @gql.field age: int,
  @gql.field lastAge: option<int>,
}

let fromDbUser = (dbUser: Db.userFromDb): user => {
  id: dbUser.id,
  name: dbUser.name,
  age: dbUser.age,
  lastAge: None,
}
