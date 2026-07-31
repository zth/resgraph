import assert from "node:assert/strict";
import {
  execute,
  GraphQLObjectType,
  GraphQLSchema,
  GraphQLString,
  parse,
  validateSchema,
} from "../node_modules/graphql/index.js";
import {resgraphCompatPlugin, unwrapResolverSource} from "../compat.mjs";

assert.deepEqual(unwrapResolverSource({_0: {value: "record"}}), {value: "record"});
assert.deepEqual(unwrapResolverSource({VAL: {value: "variant"}}), {value: "variant"});
assert.deepEqual(unwrapResolverSource({value: "plain"}), {value: "plain"});

const wrappedType = new GraphQLObjectType({
  name: "CompatWrapped",
  fields: {value: {type: GraphQLString}},
});

const queryType = new GraphQLObjectType({
  name: "CompatQuery",
  fields: {
    record: {type: wrappedType, resolve: () => ({_0: {value: "record"}})},
    variant: {type: wrappedType, resolve: () => ({VAL: {value: "variant"}})},
  },
});

const originalSchema = new GraphQLSchema({query: queryType});
let schema;
resgraphCompatPlugin().onSchemaChange({
  schema: originalSchema,
  replaceSchema: replacement => {
    schema = replacement;
  },
});

assert.deepEqual(validateSchema(schema), []);
const result = await execute({
  schema,
  document: parse(`{ record { value } variant { value } }`),
});
assert.equal(result.errors, undefined);
assert.deepEqual(JSON.parse(JSON.stringify(result.data)), {
  record: {value: "record"},
  variant: {value: "variant"},
});

console.log("✅ The shipped compatibility plugin unwraps merged resolver sources.");
