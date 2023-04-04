module UserLoader = DataLoader.Make({
  type t = option<DB.User.t>
  type key = string
})

type context = {
  currentUserId: option<string>,
  loadCurrentUser: unit => promise<option<DB.User.t>>,
  userLoader: UserLoader.t,
}
