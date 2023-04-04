module type Config = {
  type t
  type key
}

module Make = (Config: Config) => {
  type t
  type key = Config.key
  type value = Config.t

  type batchFn = array<key> => promise<array<value>>

  @module @new external createLoader: batchFn => t = "dataloader"
  @send external load: (t, key) => promise<value> = "load"
  @send external loadMany: (t, array<key>) => promise<array<value>> = "loadMany"
}
