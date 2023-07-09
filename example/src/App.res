@val external process: 'a = "process"

open GraphQLYoga

let yoga = createYoga({
  schema: ResGraphSchema.schema,
  context: async ({request}) => {
    open ResGraphContext

    {
      currentUserId: request->Request.headers->Headers.get("x-user-id"),
    }
  },
})

let server = NodeHttpServer.createServer(yoga)

server->NodeHttpServer.listen(9797, () => {
  Console.info("Server is running on http://localhost:9797/graphql")
})
