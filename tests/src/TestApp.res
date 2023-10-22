@val external process: 'a = "process"

let shouldDumpSchema = process["env"]["DUMP_SCHEMA"] === "true"

if shouldDumpSchema {
  Console.log("Dumping schema...")

  open NodeJs

  Fs.writeFileSync(
    Path.resolve([Process.cwd(Process.process), "./schema.graphql"]),
    ResGraph.printSchema(ResGraphSchema.schema)->Buffer.fromString,
  )
}

open GraphQLYoga

let yoga = createYoga({
  schema: ResGraphSchema.schema,
  context: async ({request}) => {
    ignore(request)
    {
      ResGraphContext.currentUserId: Some("123"),
      dataLoaders: {
        user: UserDataLoaders.make(),
      },
    }
  },
})
let server = NodeHttpServer.createServer(yoga)

server->NodeHttpServer.listen(9898, () => {
  Console.info("Server is running on http://localhost:9898/graphql")
})
