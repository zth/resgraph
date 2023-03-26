var express = require("express");
var { graphqlHTTP } = require("express-graphql");
var { printSchema } = require("graphql");
var fs = require("fs");
var path = require("path");

var { schema } = require("../lib/js/src/ResGraphSchema.js");

var shouldDumpSchema = Boolean(process.env.DUMP_SCHEMA);

if (shouldDumpSchema) {
  fs.writeFileSync(
    path.resolve(process.cwd(), "./schema.graphql"),
    printSchema(schema)
  );
}

var app = express();
app.use(
  "/graphql",
  graphqlHTTP({
    schema: schema,
    graphiql: true,
  })
);
app.listen(9898);
console.log("Running a GraphQL API server at http://localhost:9898/graphql");
