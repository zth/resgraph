import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { getDirective, getDirectives } from "@graphql-tools/utils";
import { execute, parse, validateSchema } from "graphql";
import { schema } from "./src/__generated__/ResGraphSchema.mjs";

const plain = value => JSON.parse(JSON.stringify(value));

assert.doesNotThrow(() => parse(readFileSync("./src/__generated__/schema.graphql", "utf8")));
assert.deepEqual(validateSchema(schema), []);

const cacheControl = schema.getDirective("cacheControl");
assert.ok(cacheControl);
assert.deepEqual(cacheControl.locations, ["OBJECT", "FIELD_DEFINITION"]);
assert.equal(cacheControl.isRepeatable, false);
assert.equal(cacheControl.args.find(argument => argument.name === "maxAge").defaultValue, 60);
assert.equal(
  cacheControl.args.find(argument => argument.name === "legacyScope").deprecationReason,
  "Use scope instead.",
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

console.log("✅ Directive definitions, metadata, ordering, and execution work.");
