@gql.type
type user = {
  id: string,
  name: string,
  age: int,
  lastAge: option<int>,
}

let fromDbUser = (dbUser: Db.userFromDb): user => {
  id: dbUser.id,
  name: dbUser.name,
  age: dbUser.age,
  lastAge: None,
}
