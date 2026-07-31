import assert from "node:assert/strict";
import { coerceInputValue, graphql } from "graphql";
import { schema } from "./src/__generated__/ResGraphSchema.mjs";

const inputType = schema.getType("Res12Input");
assert.ok(inputType);
assert.equal(inputType.isOneOf, true);
assert.equal(inputType.extensions?.oneOf, undefined);

const introspection = await graphql({
  schema,
  source: `query { __type(name: "Res12Input") { isOneOf } }`,
});
assert.equal(introspection.errors, undefined);
assert.deepEqual(JSON.parse(JSON.stringify(introspection.data)), {
  __type: { isOneOf: true },
});

const coerce = value => {
  const errors = [];
  const result = coerceInputValue(value, inputType, (_path, _value, error) => {
    errors.push(error.message);
  });
  return {result, errors};
};

assert.deepEqual(coerce({inline: {payload: "native"}}).errors, []);
assert.equal(coerce({}).errors.length, 1);
assert.equal(
  coerce({inline: {payload: "native"}, empty: true}).errors.length,
  1,
);
assert.equal(coerce({empty: null}).errors.length, 1);

console.log("✅ Native OneOf introspection and coercion work without a plugin.");
