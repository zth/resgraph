module Chokidar = {
  type t

  module Watcher = {
    type t

    @send
    external onChange: (t, @as(json`"change"`) _, string => unit) => t = "on"

    @send
    external onUnlink: (t, @as(json`"unlink"`) _, string => unit) => t = "on"

    @send
    external onAdd: (t, @as(json`"add"`) _, string => unit) => t = "on"

    @send
    external close: t => Promise.t<unit> = "close"
  }

  @module("chokidar") @val
  external watcher: t = "default"

  @send
  external watch: (t, string) => Watcher.t = "watch"
}
