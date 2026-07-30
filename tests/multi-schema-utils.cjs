const assert = require("node:assert/strict");
const {resolve} = require("node:path");
const {pathToFileURL} = require("node:url");

const testsDir = __dirname;

const run = async () => {
  const {schemaForGraphqlFile} = await import(
    pathToFileURL(resolve(testsDir, "../cli/Utils.mjs")).href
  );
  const generated = resolve(testsDir, "multi-schema/src/generated");
  const broad = {name: "broad", outputFolder: generated};
  const nested = {name: "nested", outputFolder: resolve(generated, "admin")};
  const config = {schemas: [broad, nested], defaultSchema: "broad"};

  assert.equal(
    schemaForGraphqlFile(config, resolve(nested.outputFolder, "schema.graphql")),
    nested,
  );
  console.log("multi-schema utility regressions passed");
};

run().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
