import assert from "node:assert/strict";
import {execFileSync} from "node:child_process";
import {readFile, readdir} from "node:fs/promises";
import {createRequire} from "node:module";
import path from "node:path";
import {fileURLToPath, pathToFileURL} from "node:url";

const testDirectory = path.dirname(fileURLToPath(import.meta.url));
const packageRoot = path.resolve(testDirectory, "..");
const commonJsPath = path.join(packageRoot, "src/res/ResGraph.js");
const esModulePath = path.join(packageRoot, "src/res/ResGraph.mjs");
const require = createRequire(import.meta.url);

const commonJsRuntime = require(commonJsPath);
const esModuleRuntime = await import(pathToFileURL(esModulePath));

assert.ok(Object.keys(commonJsRuntime).length > 0, "CommonJS runtime is empty");
assert.ok(Object.keys(esModuleRuntime).length > 0, "ESM runtime is empty");

const commonJsDataLoader = require(path.join(packageRoot, "src/res/DataLoader.js"));
const esModuleDataLoader = await import(
  pathToFileURL(path.join(packageRoot, "src/res/DataLoader.mjs")),
);

assert.equal(
  await commonJsDataLoader.load(
    commonJsDataLoader.makeSingle(async (key) => `commonjs:${key}`),
    "loaded",
  ),
  "commonjs:loaded",
);
assert.equal(
  await esModuleDataLoader.load(
    esModuleDataLoader.makeSingle(async (key) => `esm:${key}`),
    "loaded",
  ),
  "esm:loaded",
);

const commonJsConnections = require(
  path.join(packageRoot, "src/res/ResGraph__Connections.js"),
);
const commonJsUtils = require(
  path.join(packageRoot, "src/res/ResGraph__Utils.js"),
);
const connection = commonJsConnections.connectionFromArray(
  ["first", "second"],
  {after: undefined, before: undefined, first: 1, last: undefined},
);
assert.deepEqual(connection.edges.map(({node}) => node), ["first"]);
assert.equal(connection.pageInfo.hasNextPage, true);
assert.equal(
  commonJsUtils.Base64.decode(commonJsUtils.Base64.encode("ResGraph ✓")),
  "ResGraph ✓",
);

const runtimeDirectory = path.join(packageRoot, "src/res");
const commonJsFiles = (await readdir(runtimeDirectory))
  .filter((fileName) => fileName.endsWith(".js"))
  .map((fileName) => path.join(runtimeDirectory, fileName));
for (const filePath of commonJsFiles) {
  const source = await readFile(filePath, "utf8");
  assert.doesNotMatch(
    source,
    /require\s*\([^)]*\.mjs["']/,
    `${path.relative(packageRoot, filePath)} requires an ESM module`,
  );
}

const packOutput = execFileSync(
  "npm",
  ["pack", "--dry-run", "--json", "--ignore-scripts"],
  {cwd: packageRoot, encoding: "utf8"},
);
const [{files}] = JSON.parse(packOutput);
const packedFiles = new Set(files.map(({path: filePath}) => filePath));

assert.ok(
  packedFiles.has("src/res/ResGraph.js"),
  "npm package is missing CommonJS runtime",
);
assert.ok(
  packedFiles.has("src/res/ResGraph.mjs"),
  "npm package is missing ESM runtime",
);
for (const helperPath of [
  "src/res/dataLoaderCompat.cjs",
  "src/res/graphqlRelayConnections.cjs",
  "src/res/stableStringify.cjs",
]) {
  assert.ok(packedFiles.has(helperPath), `npm package is missing ${helperPath}`);
}

console.log("Verified ESM and CommonJS runtime behavior and package outputs.");
