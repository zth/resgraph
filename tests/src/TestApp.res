@val external process: 'a = "process"

let shouldDumpSchema = process["env"]["DUMP_SCHEMA"] === "true"

if shouldDumpSchema {
  open Node

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
    open ResGraphContext
    ignore(request)
    let currentUserId = "1"
    let userLoader = UserLoader.createLoader(DB.User.findMany)
    {
      currentUserId: Some(currentUserId),
      loadCurrentUser: () => userLoader->UserLoader.load(currentUserId),
      userLoader,
    }
  },
})
let server = NodeHttpServer.createServer(yoga)

server->NodeHttpServer.listen(9898, () => {
  Console.info("Server is running on http://localhost:9898/graphql")
})
