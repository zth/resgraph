import {readFile} from "node:fs/promises";
import path from "node:path";
import {fileURLToPath} from "node:url";

const repositoryRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const requestedTag = process.argv[2];

if (requestedTag === undefined) {
  throw new Error("Usage: node scripts/verify-release.mjs <tag>");
}

const version = requestedTag.startsWith("v") ? requestedTag.slice(1) : requestedTag;
const packageJson = JSON.parse(await readFile(path.join(repositoryRoot, "package.json"), "utf8"));
const changelog = await readFile(path.join(repositoryRoot, "CHANGELOG.md"), "utf8");

if (packageJson.version !== version) {
  throw new Error(
    `Release tag ${requestedTag} does not match package version ${packageJson.version}.`,
  );
}

if (!changelog.includes(`## ${version}\n`)) {
  throw new Error(`CHANGELOG.md does not contain a section for ${version}.`);
}

console.log(`Release metadata is consistent for ${requestedTag}.`);
