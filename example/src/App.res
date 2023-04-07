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

module Yoga = GraphQLYoga.MakeYoga({
  type appContext = ResGraphContext.context
})

open Yoga

let yoga = createYoga({
  schema: ResGraphSchema.schema,
  context: async ({request}) => {
    ignore(request)
    {
      ResGraphContext.currentUserId: Some("123"),
    }
  },
})
let server = NodeHttpServer.createServer(yoga)

server->NodeHttpServer.listen(9797, () => {
  Console.info("Server is running on http://localhost:9797/graphql")
})
