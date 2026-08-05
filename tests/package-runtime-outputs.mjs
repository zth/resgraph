import assert from "node:assert/strict";
import {execFileSync} from "node:child_process";
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

console.log("Verified ESM and CommonJS runtime package outputs.");
