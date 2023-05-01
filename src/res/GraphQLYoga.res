module Request = Fetch.Request
module Headers = Fetch.Headers
module Body = Fetch.Body
module FormData = Fetch.FormData
module Blob = Fetch.Blob

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

// This is the bare minimum right now, but should be extended with all options here eventually:
// https://github.com/dotansimha/graphql-yoga/blob/main/packages/graphql-yoga/src/server.ts#L85
type createServerConfig<'appContext> = {
  schema: ResGraph.schema<'appContext>,
  context: contextConfig => promise<'appContext>,
}

module NodeHttpServer = {
  type t

  @module("node:http") external createServer: Server.t => t = "createServer"

  @send external listen: (t, int, unit => unit) => unit = "listen"
}

@module("graphql-yoga")
external createYoga: createServerConfig<'appContext> => Server.t = "createYoga"
