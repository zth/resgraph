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
    ignore(request)
    {
      ResGraphContext.currentUserId: Some("123"),
      loadCurrentUser: async () => Some({
        id: "123",
        User.name: "TestUser",
        age: 35,
        lastAge: None,
      }),
      userById: async (~userId) => Some({
        id: userId,
        User.name: "Testing testing",
        age: 37,
        lastAge: None,
      }),
    }
  },
})
let server = NodeHttpServer.createServer(yoga)

server->NodeHttpServer.listen(9898, () => {
  Console.info("Server is running on http://localhost:9898/graphql")
})
