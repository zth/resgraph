type dataLoaders = {user: UserDataLoaders.t}

type context = {
  currentUserId: option<string>,
  dataLoaders: dataLoaders,
}
