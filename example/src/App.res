@val external process: 'a = "process"

let shouldDumpSchema = process["env"]["DUMP_SCHEMA"] === "true"

if shouldDumpSchema {
  open Node

  Console.log("Dumping schema.")

  Fs.writeFileAsUtf8Sync(
    Path.resolve(Process.cwd(), "./schema.graphql"),
    ResGraph.printSchema(ResGraphSchema.schema),
  )
}

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
