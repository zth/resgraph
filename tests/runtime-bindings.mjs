import assert from "node:assert/strict";
import {
  GraphQLInt,
  GraphQLObjectType,
  GraphQLSchema,
  GraphQLString,
} from "../node_modules/graphql/index.js";
import * as DataLoader from "../src/res/DataLoader.mjs";
import {Execute} from "../src/res/ResGraph.mjs";
import {stableStringify} from "../src/res/stableStringify.mjs";

assert.notEqual(
  stableStringify([1, 2]),
  stableStringify([2, 1]),
  "stable DataLoader keys must preserve array order",
);

let loadCount = 0;
const loader = DataLoader.makeBatched(async keys => {
  loadCount += keys.length;
  return keys.map(key =>
    key === "error"
      ? new Error("expected loader error")
      : `loaded:${key}`,
  );
});

DataLoader.primeAt(loader, "primed", "from-cache");
assert.equal(await DataLoader.load(loader, "primed"), "from-cache");
assert.equal(loadCount, 0, "primeAt must populate the requested key");

const loadManyResults = await DataLoader.loadManyResults(loader, ["ok", "error"]);
assert.deepEqual(loadManyResults[0], {TAG: "Ok", _0: "loaded:ok"});
assert.equal(loadManyResults[1].TAG, "Error");
assert.match(loadManyResults[1]._0.message, /expected loader error/);

const typedResultsLoader = DataLoader.makeBatchedResults(async keys =>
  keys.map(key =>
    key === "error"
      ? {TAG: "Error", _0: new Error("typed loader error")}
      : {TAG: "Ok", _0: `typed:${key}`},
  ),
);
const typedResults = await DataLoader.loadManyResults(typedResultsLoader, [
  "ok",
  "error",
]);
assert.deepEqual(typedResults[0], {TAG: "Ok", _0: "typed:ok"});
assert.match(typedResults[1]._0.message, /typed loader error/);

const queryType = new GraphQLObjectType({
  name: "Query",
  fields: {
    greeting: {
      type: GraphQLString,
      resolve: () => "hello",
    },
    echo: {
      type: GraphQLInt,
      args: {value: {type: GraphQLInt}},
      resolve: (_source, args) => args.value,
    },
  },
});
const schema = new GraphQLSchema({query: queryType});

const synchronousResult = await Execute.executeToJson(schema, "{ greeting }", undefined);
assert.equal(synchronousResult.data.greeting, "hello");

const boundedCache = Execute.makeQueryDocumentCacheWithMaxSize(1);
const firstQuery = "{ greeting }";
const secondQuery = "query EchoAgain { echo(value: 7) }";
Execute.parseQueryCached(boundedCache, firstQuery);
Execute.parseQueryCached(boundedCache, secondQuery);
assert.equal(Execute.getCachedQuery(boundedCache, firstQuery), undefined);
assert.notEqual(Execute.getCachedQuery(boundedCache, secondQuery), undefined);
assert.throws(() => Execute.makeQueryDocumentCacheWithMaxSize(0), /positive integer/);

const query = "query Echo($value: Int) { echo(value: $value) }";
const cache = Execute.makeQueryDocumentCache();
const variablesResult = await Execute.executeToJson(schema, query, undefined, cache, {value: 42});
assert.equal(variablesResult.data.echo, 42);
assert.notEqual(Execute.getCachedQuery(cache, query), undefined);

const invalidQueryResult = await Execute.executeToJson(schema, "{ missing }", undefined);
assert.match(invalidQueryResult.errors[0].message, /Cannot query field "missing"/);

assert.deepEqual(
  await Execute.executeToJson(schema, "{ greeting }", undefined, undefined, []),
  {errors: [{message: "GraphQL variables must be a JSON object or null."}]},
);

console.log("Runtime binding fixtures passed.");
