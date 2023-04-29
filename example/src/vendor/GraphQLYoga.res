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

type createServerConfig<'appContext> = {
  schema: ResGraph.schema<'appContext>,
  context: contextConfig => promise<'appContext>,
}

module NodeHttpServer = {
  type t

  @module("node:http") external createServer: Server.t => t = "createServer"

  @send external listen: (t, int, unit => unit) => unit = "listen"
}

external createYoga: createServerConfig<'appContext> => Server.t = "createYoga"
