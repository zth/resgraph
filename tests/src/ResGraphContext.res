type context = {currentUserId: option<string>, loadCurrentUser: unit => promise<option<User.user>>}
