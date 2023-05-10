module Plain = {
  type t<'key, 'value>

  type options = {
    /**
    * Default `true`. Set to `false` to disable batching, invoking
    * `batchLoadFn` with a single load key. This is equivalent to setting
    * `maxBatchSize` to `1`.
    */
    batch?: bool,
    /**
    * Default `Infinity`. Limits the number of items that get passed in to the
    * `batchLoadFn`. May be set to `1` to disable batching.
    */
    maxBatchSize?: int,
    /**
    * Default see https://github.com/graphql/dataloader#batch-scheduling.
    * A function to schedule the later execution of a batch. The function is
    * expected to call the provided callback in the immediate future.
    */
    batchScheduleFn?: (unit => unit) => unit,
    /**
    * Default `true`. Set to `false` to disable memoization caching, creating a
    * new Promise and new key in the `batchLoadFn` for every load of the same
    * key. This is equivalent to setting `cacheMap` to `null`.
    */
    cache?: bool,
    /**
    * Default `key => key`. Produces cache key for a given load key. Useful
    * when keys are objects and two objects should be considered equivalent.
    */
    cacheKeyFn?: unknown => unknown,
    /**
    * The name given to this `DataLoader` instance. Useful for APM tools.
    *
    * Is `null` if not set in the constructor.
    */
    name?: string,
  }

  type batchFn<'key, 'value> = array<'key> => promise<array<'value>>

  @new @module("dataloader")
  external make: (batchFn<'key, 'value>, ~options: options=?) => t<'key, 'value> = "default"

  /**
   * Loads a key, returning a `promise` for the value represented by that key.
   */
  @send
  external load: (t<'key, 'value>, 'key) => promise<'value> = "load"

  /**
   * Loads multiple keys, promising an array of values.
   */
  @send
  external loadMany: (t<'key, 'value>, array<'key>) => promise<array<'value>> = "loadMany"

  /**
   * Clears the value at `key` from the cache, if it exists.
   */
  @send
  external clear: (t<'key, 'value>, 'key) => unit = "clear"

  /**
   * Clears the entire cache. To be used when some event results in unknown
   * invalidations across this particular `DataLoader`.
   */
  @send
  external clearAll: t<'key, 'value> => unit = "clearAll"

  /**
   * Adds the provided key and value to the cache. If the key already exists, no
   * change is made.
   */
  @send
  external prime: (t<'key, 'value>, 'value) => unit = "prime"

  /**
   * Adds the provided key and (promised) value to the cache. If the key already exists, no
   * change is made.
   */
  @send
  external primeWithPromise: (t<'key, 'value>, promise<'value>) => unit = "prime"

  /**
   * The name given to this `DataLoader` instance, if set. Useful for APM tools..
   */
  @get
  @return(nullable)
  external name: t<'key, 'value> => option<string> = "name"
}

type t<'key, 'value> = Lazy.t<Plain.t<'key, 'value>>

type batchFn<'key, 'value> = array<'key> => promise<array<'value>>

type options = {
  /**
    * Default see https://github.com/graphql/dataloader#batch-scheduling.
    * A function to schedule the later execution of a batch. The function is
    * expected to call the provided callback in the immediate future.
    */
  batchScheduleFn?: (unit => unit) => unit,
  /**
    * The name given to this `DataLoader` instance. Useful for APM tools.
    *
    * Is `null` if not set in the constructor.
    */
  name?: string,
}

@module("./stableStringify.mjs")
external stableStringifyValue: 'any => 'any = "stableStringify"

let mapOptions = options => {
  let baseOpts = {
    Plain.cacheKeyFn: stableStringifyValue,
  }

  switch options {
  | None => Some(baseOpts)
  | Some(opts) =>
    Some({
      ...baseOpts,
      Plain.batchScheduleFn: ?opts.batchScheduleFn,
      name: ?opts.name,
    })
  }
}

let makeSingle = (loadFn, ~options=?) => {
  Lazy.from_fun(() =>
    Plain.make(keys => Promise.all(keys->Array.map(loadFn)), ~options=?mapOptions(options))
  )
}

let makeBatched = (loadFn: batchFn<'key, 'value>, ~options=?) => {
  Lazy.from_fun(() => Plain.make(loadFn, ~options=?mapOptions(options)))
}

let load = (lazyLoader, key) => {
  let loader = lazyLoader->Lazy.force
  loader->Plain.load(key)
}

let loadMany = (lazyLoader, keys) => {
  let loader = lazyLoader->Lazy.force
  loader->Plain.loadMany(keys)
}

let clear = (lazyLoader, key) => {
  let loader = lazyLoader->Lazy.force
  loader->Plain.clear(key)
}

let clearAll = lazyLoader => {
  let loader = lazyLoader->Lazy.force
  loader->Plain.clearAll
}

let prime = (lazyLoader, value) => {
  let loader = lazyLoader->Lazy.force
  loader->Plain.prime(value)
}

let primeWithPromise = (lazyLoader, value) => {
  let loader = lazyLoader->Lazy.force
  loader->Plain.primeWithPromise(value)
}

let name = lazyLoader => {
  let loader = lazyLoader->Lazy.force
  loader->Plain.name
}
