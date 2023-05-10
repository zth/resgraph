/** Zero cost bindings to the standard, vanilla data loader. */
module Plain: {
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

  /** Create a new data loader, from a batch load function. */
  @new
  @module("dataloader")
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

type t<'key, 'value>

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

/** Creates a new data loader, where the load function is _not_ batched. */
let makeSingle: ('key => promise<'value>, ~options: options=?) => t<'key, 'value>

/** Creates a new data loader, where the load function is batched. */
let makeBatched: (batchFn<'key, 'value>, ~options: options=?) => t<'key, 'value>

/**
  * Loads a key, returning a `promise` for the value represented by that key.
  */
let load: (t<'key, 'value>, 'key) => promise<'value>

/**
  * Loads multiple keys, promising an array of values.
  */
let loadMany: (t<'key, 'value>, array<'key>) => promise<array<'value>>

/**
  * Clears the value at `key` from the cache, if it exists.
  */
let clear: (t<'key, 'value>, 'key) => unit

/**
  * Clears the entire cache. To be used when some event results in unknown
  * invalidations across this particular `DataLoader`.
  */
let clearAll: t<'key, 'value> => unit

/**
  * Adds the provided key and value to the cache. If the key already exists, no
  * change is made.
  */
let prime: (t<'key, 'value>, 'value) => unit

/**
  * Adds the provided key and (promised) value to the cache. If the key already exists, no
  * change is made.
  */
let primeWithPromise: (t<'key, 'value>, promise<'value>) => unit

/**
  * The name given to this `DataLoader` instance, if set. Useful for APM tools..
  */
let name: t<'key, 'value> => option<string>
