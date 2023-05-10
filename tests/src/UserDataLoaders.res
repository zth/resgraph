type t = {byId: DataLoader.t<string, result<User.user, string>>}

let make = () => {
  byId: DataLoader.makeSingle(async userId => {
    switch Db.findSingleUserById(~userId) {
    | Ok(userFromDb) => Ok(userFromDb->User.fromDbUser)
    | Error(error) => Error(error)
    }
  }),
}
