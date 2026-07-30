const assert = require("node:assert/strict");
const {mkdtempSync, mkdirSync, rmSync, symlinkSync, writeFileSync} = require("node:fs");
const {tmpdir} = require("node:os");
const {join, resolve} = require("node:path");
const {pathToFileURL} = require("node:url");

const testsDir = __dirname;

const run = async () => {
  const {schemaForFile, schemaForGraphqlFile} = await import(
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

  const temporaryRoot = mkdtempSync(join(tmpdir(), "resgraph-routing-"));
  try {
    const realSources = join(temporaryRoot, "real-sources");
    const sourceAlias = join(temporaryRoot, "source-alias");
    mkdirSync(realSources);
    symlinkSync(realSources, sourceAlias);
    const sourceFile = join(realSources, "Source.res");
    writeFileSync(sourceFile, "");
    const defaultSchema = {
      name: "default",
      projectRoot: temporaryRoot,
      includePaths: [],
      excludePaths: [realSources],
    };
    const aliasedSchema = {
      name: "aliased",
      projectRoot: temporaryRoot,
      includePaths: [sourceAlias],
      excludePaths: [],
    };
    const routingConfig = {
      schemas: [defaultSchema, aliasedSchema],
      defaultSchema: "default",
    };
    assert.equal(schemaForFile(routingConfig, sourceFile), aliasedSchema);
  } finally {
    rmSync(temporaryRoot, {recursive: true, force: true});
  }
  console.log("multi-schema utility regressions passed");
};

run().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
