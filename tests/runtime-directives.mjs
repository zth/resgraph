import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { getDirective, getDirectives } from "@graphql-tools/utils";
import { buildSchema, execute, parse, validateSchema } from "graphql";
import { schema } from "./src/__generated__/ResGraphSchema.mjs";

const plain = value => JSON.parse(JSON.stringify(value));

const sdl = readFileSync("./src/__generated__/schema.graphql", "utf8");
assert.doesNotThrow(() => parse(sdl));
assert.deepEqual(validateSchema(buildSchema(sdl)), []);
assert.deepEqual(validateSchema(schema), []);
assert.equal(schema.description, "The public ResGraph test schema.");
assert.equal(schema.getQueryType().name, "Query");
assert.deepEqual(plain(getDirective(schema, schema, "tag")), [{name: "schema"}]);
assert.match(sdl, /schema @tag\(name: "schema"\) \{/);

const cacheControl = schema.getDirective("cacheControl");
assert.ok(cacheControl);
assert.equal(cacheControl.description, 'Caching metadata consumed by a """schema transform""".');
assert.deepEqual(cacheControl.locations, ["OBJECT", "FIELD_DEFINITION"]);
assert.equal(cacheControl.isRepeatable, false);
assert.equal(cacheControl.args.find(argument => argument.name === "maxAge").defaultValue, 60);
assert.equal(
  cacheControl.args.find(argument => argument.name === "legacyScope").deprecationReason,
  'Use "scope" instead.',
);

const tag = schema.getDirective("tag");
assert.ok(tag);
assert.equal(tag.isRepeatable, true);

const directiveExample = schema.getType("DirectiveExample");
assert.deepEqual(plain(getDirective(schema, directiveExample, "tag")), [
  { name: "first" },
  { name: "second" },
]);
assert.deepEqual(
  getDirectives(schema, directiveExample).map(directive => directive.name),
  ["tag", "tag", "cacheControl"],
);
assert.deepEqual(plain(directiveExample.extensions.resgraph.appliedDirectives), [
  { name: "tag", args: { name: "first" } },
  { name: "cacheControl", args: { maxAge: 30 } },
  { name: "tag", args: { name: "second" } },
]);

const valueField = directiveExample.getFields().value;
assert.deepEqual(plain(getDirective(schema, valueField, "cacheControl")), [
  { maxAge: 10, scope: "private" },
]);

const uuid = schema.getType("Uuid");
assert.equal(uuid.specifiedByURL, "https://example.com/specifiedBy/uuid");
assert.deepEqual(plain(getDirective(schema, uuid, "tag")), [{ name: "scalar" }]);

const directiveInput = schema.getType("DirectiveInput");
assert.equal(directiveInput.getFields().label.defaultValue, "fallback");

const argumentMetadata = schema.getQueryType().getFields().directiveArgumentMetadata.args[0];
assert.equal(argumentMetadata.name, "limit");
assert.equal(argumentMetadata.defaultValue, 25);
assert.equal(argumentMetadata.description, "Maximum number of results.");
assert.equal(argumentMetadata.deprecationReason, "Use pageSize instead.");
assert.deepEqual(plain(getDirective(schema, argumentMetadata, "tag")), [
  {name: "argument"},
]);

const result = await execute({
  schema,
  document: parse(`
    query DirectiveExample($input: DirectiveInput!) {
      directiveExample(input: $input) {
        value
        status
      }
    }
  `),
  variableValues: { input: { value: "directives work" } },
});

assert.equal(result.errors, undefined);
assert.deepEqual(plain(result.data), {
  directiveExample: { value: "directives work", status: "Active" },
});

const defaultResult = await execute({
  schema,
  document: parse(`
    query DirectiveInputDefault($input: DirectiveInput!) {
      directiveInputDefault(input: $input)
    }
  `),
  variableValues: { input: { value: "provided" } },
});

assert.equal(defaultResult.errors, undefined);
assert.deepEqual(plain(defaultResult.data), {
  directiveInputDefault: "fallback",
});

const argumentDefaultResult = await execute({
  schema,
  document: parse(`query { directiveArgumentMetadata }`),
});

assert.equal(argumentDefaultResult.errors, undefined);
assert.deepEqual(plain(argumentDefaultResult.data), {
  directiveArgumentMetadata: 25,
});

const asyncValuesField = schema.getQueryType().getFields().asyncValues;
assert.equal(asyncValuesField.type.toString(), "[String!]!");
const asyncValues = await asyncValuesField.resolve(undefined, {}, {}, {});
assert.equal(typeof asyncValues[Symbol.asyncIterator], "function");

const shorthandQueryResult = await execute({
  schema,
  document: parse(`{
    shorthandGreeting
    shorthandEcho(message: "echo")
    shorthandContext
  }`),
  contextValue: {},
});

assert.equal(shorthandQueryResult.errors, undefined);
assert.deepEqual(plain(shorthandQueryResult.data), {
  shorthandGreeting: "hello",
  shorthandEcho: "echo",
  shorthandContext: "context",
});

const shorthandMutationResult = await execute({
  schema,
  document: parse(`mutation { shorthandIncrement(value: 2) }`),
});

assert.equal(shorthandMutationResult.errors, undefined);
assert.deepEqual(plain(shorthandMutationResult.data), {
  shorthandIncrement: 3,
});

const scalarLiteralResult = await execute({
  schema,
  document: parse(`{ literalText(value: "literal") }`),
});

assert.equal(scalarLiteralResult.errors, undefined);
assert.deepEqual(plain(scalarLiteralResult.data), {
  literalText: "literal",
});

const scalarVariableResult = await execute({
  schema,
  document: parse(`query($value: LiteralText!) { literalText(value: $value) }`),
  variableValues: {value: "variable"},
});

assert.equal(scalarVariableResult.errors, undefined);
assert.deepEqual(plain(scalarVariableResult.data), {
  literalText: "variable",
});

console.log("✅ Directive definitions, metadata, ordering, and execution work.");
