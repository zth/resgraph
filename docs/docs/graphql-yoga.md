# GraphQL Yoga

ResGraph comes with first class support for [GraphQL Yoga](https://the-guild.dev/graphql/yoga-server), a popular and capable GraphQL backend for JavaScript.

The bindings to GraphQL Yoga lives in `GraphQLYoga.res`.

In general, if you find something is missing when using GraphQL Yoga, feel free to open an [issue on the repository](https://github.com/zth/resgraph/issues). Support GraphQL Yoga is high priority.

## Using Envelope plugins

Yoga integrates tightly with [Envelope](https://the-guild.dev/graphql/envelop), a plugin system for GraphQL. ResGraph does not ship with bindings to any Envelope plugins by default. It's however trivial to bind to and use plugins yourself if you want. Here's an example of how you can do that:

```rescript
type useRateLimiterConfig = {
    // ... whatever config the plugin has goes here
}

@module("@envelop/rate-limiter")
external useRateLimiter: useRateLimiterConfig => GraphQLYoga.Envelope.plugin = "useRateLimiter"
```

You then pass it to the `plugins` option when calling `createYoga`:

```rescript
let yoga = createYoga({
  schema: ResGraphSchema.schema,
  plugins: [
    useRateLimiter({
      // ... plugin options
    })
  ]
})
```
