import assert from "node:assert/strict";
import {access, chmod, copyFile, mkdir, mkdtemp, rm, writeFile} from "node:fs/promises";
import {tmpdir} from "node:os";
import path from "node:path";
import {fileURLToPath} from "node:url";
import {spawnSync} from "node:child_process";

const repositoryRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const temporaryRoot = await mkdtemp(path.join(tmpdir(), "resgraph-package-"));
const consumerRoot = path.join(temporaryRoot, "consumer");

function run(command, args, options = {}) {
  const result = spawnSync(command, args, {
    cwd: options.cwd ?? repositoryRoot,
    encoding: "utf8",
    env: process.env,
    stdio: options.capture ? "pipe" : "inherit",
  });
  if (result.status !== 0) {
    const output = [result.stdout, result.stderr].filter(Boolean).join("\n");
    throw new Error(`${command} ${args.join(" ")} failed with status ${result.status}.\n${output}`);
  }
  return result.stdout;
}

async function exists(filePath) {
  try {
    await access(filePath);
    return true;
  } catch {
    return false;
  }
}

const platformFolder =
  process.platform === "darwin"
    ? process.arch === "arm64"
      ? "darwinarm64"
      : "darwin"
    : process.platform === "linux" && process.arch === "x64"
      ? "linux"
      : undefined;

if (platformFolder === undefined) {
  throw new Error(`Packed-package fixture does not support ${process.platform}/${process.arch}.`);
}

const packagedBinaryDirectory = path.join(repositoryRoot, "bin", platformFolder);
const packagedBinary = path.join(packagedBinaryDirectory, "resgraph.exe");
const developmentBinary = path.join(repositoryRoot, "bin", "dev", "resgraph.exe");
let copiedDevelopmentBinary = false;

try {
  if (!(await exists(packagedBinary))) {
    if (!(await exists(developmentBinary))) {
      throw new Error("Build bin/dev/resgraph.exe before running the package fixture.");
    }
    await mkdir(packagedBinaryDirectory, {recursive: true});
    await copyFile(developmentBinary, packagedBinary);
    await chmod(packagedBinary, 0o755);
    copiedDevelopmentBinary = true;
  }

  const packOutput = run(
    "npm",
    ["pack", "--json", "--pack-destination", temporaryRoot],
    {capture: true},
  );
  const packJsonStart = packOutput.lastIndexOf("[\n  {");
  if (packJsonStart < 0) {
    throw new Error(`Could not find npm pack JSON in output:\n${packOutput}`);
  }
  const [{filename}] = JSON.parse(packOutput.slice(packJsonStart));
  const tarballPath = path.join(temporaryRoot, filename);

  await mkdir(path.join(consumerRoot, "src", "generated"), {recursive: true});
  await writeFile(
    path.join(consumerRoot, "package.json"),
    JSON.stringify({name: "resgraph-package-consumer", private: true, type: "module"}, null, 2) + "\n",
  );

  run(
    "npm",
    [
      "install",
      "--ignore-scripts",
      tarballPath,
      "rescript@12.0.0",
      "graphql@16.14.2",
      "@glennsl/rescript-fetch@0.2.2",
      "dataloader@2.2.2",
    ],
    {cwd: consumerRoot},
  );

  await writeFile(
    path.join(consumerRoot, "rescript.json"),
    JSON.stringify(
      {
        name: "resgraph-package-consumer",
        uncurried: true,
        sources: [{dir: "src", subdirs: true}],
        "package-specs": {module: "esmodule", "in-source": true},
        suffix: ".mjs",
        dependencies: ["resgraph"],
      },
      null,
      2,
    ) + "\n",
  );
  await writeFile(
    path.join(consumerRoot, "resgraph.json"),
    JSON.stringify(
      {
        src: "./src",
        outputFolder: "./src/generated",
        dumpSchemaSdl: true,
      },
      null,
      2,
    ) + "\n",
  );
  await writeFile(
    path.join(consumerRoot, "src", "Query.res"),
    `@gql.type
type query

@gql.field
let greeting = (_: query): string => "hello from package"
`,
  );
  await writeFile(path.join(consumerRoot, "src", "ResGraphContext.res"), "type context = unit\n");

  run(path.join(consumerRoot, "node_modules", ".bin", "rescript"), [], {cwd: consumerRoot});
  run(path.join(consumerRoot, "node_modules", ".bin", "resgraph"), ["build"], {
    cwd: consumerRoot,
  });
  run(path.join(consumerRoot, "node_modules", ".bin", "rescript"), [], {cwd: consumerRoot});

  await writeFile(
    path.join(consumerRoot, "verify.mjs"),
    `import assert from "node:assert/strict";
import * as DataLoader from "resgraph/src/res/DataLoader.mjs";
import {Execute} from "resgraph/src/res/ResGraph.mjs";
import {schema} from "./src/generated/ResGraphSchema.mjs";

const result = await Execute.executeToJson(schema, "{ greeting }", undefined);
assert.equal(result.data.greeting, "hello from package");

const loader = DataLoader.makeSingle(async key => "loaded:" + key);
DataLoader.primeAt(loader, "key", "primed");
assert.equal(await DataLoader.load(loader, "key"), "primed");
`,
  );
  run("node", ["verify.mjs"], {cwd: consumerRoot});

  const installedPackage = JSON.parse(
    run(
      "node",
      ["-e", "process.stdout.write(JSON.stringify(require('./node_modules/resgraph/package.json')))"],
      {cwd: consumerRoot, capture: true},
    ),
  );
  assert.equal(installedPackage.license, "MIT");
  assert.equal(installedPackage.main, undefined);
  assert.equal(
    await exists(path.join(consumerRoot, "node_modules", "resgraph", "resgraph.schema.json")),
    true,
  );

  console.log("Packed-package consumer fixture passed.");
} finally {
  await rm(temporaryRoot, {recursive: true, force: true});
  if (copiedDevelopmentBinary) {
    await rm(packagedBinary, {force: true});
    await rm(packagedBinaryDirectory, {recursive: true, force: true});
  }
}
