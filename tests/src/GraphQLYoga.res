module Server = {
  type t
}

module NodeHttpServer = {
  type t

  @module("node:http") external createServer: Server.t => t = "createServer"

  @send external listen: (t, int, unit => unit) => unit = "listen"
}

type request

type contextConfig = {request: request}

type createServerConfig<'appContext> = {
  schema?: ResGraph.schema<'appContext>,
  context?: contextConfig => promise<'appContext>,
}

@module("graphql-yoga")
external createYoga: createServerConfig<ResGraphContext.context> => Server.t = "createYoga"
