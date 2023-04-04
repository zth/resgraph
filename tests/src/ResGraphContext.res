type context = {
  currentUserId: option<string>,
  loadCurrentUser: unit => promise<option<User.user>>,
  userById: (~userId: string) => promise<option<User.user>>,
}
