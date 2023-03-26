var express = require("express");
var { graphqlHTTP } = require("express-graphql");

var { schema } = require("../lib/js/src/ResGraphSchema.js");

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
