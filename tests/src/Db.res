type userFromDb = {
  id: string,
  name: string,
  age: int,
}

let mockUsers = [{id: "111", name: "First User", age: 35}]

let findSingleUserById = (~userId) => {
  switch mockUsers->Array.find(u => u.id === userId) {
  | None => Error("Did not find.")
  | Some(user) => Ok(user)
  }
}
