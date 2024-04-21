module Request = Fetch.Request
module Headers = Fetch.Headers
module Body = Fetch.Body
module FormData = Fetch.FormData
module Blob = Fetch.Blob

module GraphQLError = {
  type t
  type options<'extensions> = {extensions?: {..} as 'extensions}

  @module("graphql") @new
  external make: (string, ~options: options<'extensions>=?) => t = "GraphQLError"

  let raise: t => 'a = err => raise(Obj.magic(err))
}

module Envelope = {
  type plugin

  module Plugin = {
    type t

    module ExtendedValidation = {
      module Rule = {
        type t

        @module("@envelop/extended-validation")
        external oneOfInputObjectsRule: t = "OneOfInputObjectsRule"
      }

      type config = {rules: array<Rule.t>}

      @module("@envelop/extended-validation")
      external use: config => t = "useExtendedValidation"
    }
  }
}

module Server = {
  type t
}

type graphqlParams = {
  query?: string,
  operationName?: string,
  variables?: Dict.t<ResGraph.JSON.t>,
  extensions?: Dict.t<ResGraph.JSON.t>,
}

type contextConfig = {request: Request.t, params: graphqlParams}

type logFn = unknown => unit
type yogaLogger = {debug: logFn, info: logFn, warn: logFn, error: logFn}

@unboxed
type logging =
  | @as(true) True
  | @as(false) False
  | YogaLogger(yogaLogger)
  | @as("debug") LogLevelDebug
  | @as("info") LogLevelInfo
  | @as("warn") LogLevelWarn
  | @as("error") LogLevelError

type maskErrorFn = (~error: Exn.t, ~message: string, ~isDev: option<bool>) => Exn.t
type maskedErrorOpts = {
  maskError?: maskErrorFn,
  errorMessage?: string,
  isDev?: bool,
}

@unboxed
type maskedErrors =
  | @as(true) True
  | @as(false) False
  | Options(maskedErrorOpts)

type corsOptions = {
  origin?: array<string>,
  methods?: array<string>,
  allowedHeaders?: array<string>,
  exposedHeaders?: array<string>,
  credentials?: bool,
  maxAge?: float,
}

@unboxed
type corsConfig = | @as(true) True | @as(false) False | Options(corsOptions)

type requestCredentials =
  | @as("include") Include | @as("omit") Omit | @as("same-origin") SameOrigin

type graphiQLOptions = {
  /**
   * An optional GraphQL string to use when no query is provided and no stored
   * query exists from a previous session.  If undefined is provided, GraphiQL
   * will use its own default query.
   */
  defaultQuery?: string,
  /**
   * The initial headers to render inside the header editor. Defaults to `"{}"`.
   * The value should be a JSON encoded string, for example:
   * `headers: JSON.stringify({Authorization: "Bearer your-auth-key"})`
   */
  headers?: string,
  /**
   * More info there: https://developer.mozilla.org/en-US/docs/Web/API/Request/credentials
   */
  credentials?: requestCredentials,
  /**
   * The title to display at the top of the page. Defaults to `"Yoga GraphiQL"`.
   */
  title?: string,
  /**
   * Protocol for subscriptions
   */
  subscriptionsProtocol?: [#SSE | #GRAPHQL_SSE | #WS | #LEGACY_WS],
  /**
   * Extra headers you always want to pass with users' headers input
   */
  additionalHeaders?: Dict.t<string>,
  /**
   * HTTP method to use when querying the original schema.
   */
  method?: [#GET | #POST],
  /**
   * Whether to use the GET HTTP method for queries when querying the original schema
   */
  useGETForQueries?: bool,
}

@unboxed
type graphiqlConfig = | @as(true) True | @as(false) False | Options(graphiQLOptions)

type batchingOptions = {
  /**
   * You can limit the number of batched operations per request.
   *
   * @default 10
   */
  limit?: int,
}

@unboxed type batchingConfig = | @as(true) True | @as(false) False | Options(batchingOptions)

// This is the bare minimum right now, but should be extended with all options here eventually:
// https://github.com/dotansimha/graphql-yoga/blob/main/packages/graphql-yoga/src/server.ts#L85
type createServerConfig<'appContext> = {
  schema: ResGraph.schema<'appContext>,
  /**
   * Context
   */
  context: contextConfig => promise<'appContext>,
  /**
   * Enable/disable logging or provide a custom logger.
   * @default true
   */
  logging?: logging,
  /**
   * Prevent leaking unexpected errors to the client. We highly recommend enabling this in production.
   * If you throw `EnvelopError`/`GraphQLError` within your GraphQL resolvers then that error will be sent back to the client.
   *
   * You can lean more about this here:
   * @see https://graphql-yoga.vercel.app/docs/features/error-masking
   *
   * @default true
   */
  maskedErrors?: maskedErrors,
  cors?: corsConfig,
  /**
   * GraphQL endpoint
   * So you need to define it explicitly if GraphQL API lives in a different path other than `/graphql`
   *
   * @default "/graphql"
   */
  graphqlEndpoint?: string,
  /**
   * Readiness check endpoint
   *
   * @default "/health"
   */
  healthCheckEndpoint?: string,
  /**
   * Whether the landing page should be shown.
   */
  landingPage?: bool,
  /**
   * GraphiQL options
   */
  graphiql?: graphiqlConfig,
  /**
   * Envelop Plugins
   * @see https://envelop.dev/plugins
   */
  plugins?: array<Envelope.Plugin.t>,
  parserAndValidationCache?: bool,
  /**
   * GraphQL Multipart Request spec support
   *
   * @see https://github.com/jaydenseric/graphql-multipart-request-spec
   *
   * @default true
   */
  multipart?: bool,
  id?: string,
  /**
   * Batching RFC Support configuration
   *
   * @see https://github.com/graphql/graphql-over-http/blob/main/rfcs/Batching.md
   *
   * @default false
   */
  batching?: batchingConfig,
  /**
   * Whether to use the legacy Yoga Server-Sent Events and not
   * the GraphQL over SSE spec's distinct connection mode.
   *
   * @default true
   */
  @deprecated(
    "Consider using GraphQL over SSE spec instead by setting this to `false`. Starting with the next major release, this flag will default to `false`."
  )
  legacySse?: bool,
}

module NodeHttpServer = {
  type t

  @module("node:http") external createServer: Server.t => t = "createServer"

  @send external listen: (t, int, unit => unit) => unit = "listen"
}

@module("graphql-yoga")
external createYoga: createServerConfig<'appContext> => Server.t = "createYoga"
