module type MakeYogaConfig = {
  type appContext
}

module MakeYoga = (C: MakeYogaConfig) => {
  type appContext = C.appContext

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

  type createServerConfig = {
    schema?: ResGraph.schema<appContext>,
    context?: contextConfig => promise<appContext>,
  }

  @module("graphql-yoga")
  external createYoga: createServerConfig => Server.t = "createYoga"
}
