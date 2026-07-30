const {spawn} = require("node:child_process");
const {readFileSync} = require("node:fs");
const {resolve} = require("node:path");
const {pathToFileURL} = require("node:url");

const testsDir = __dirname;
const cli = resolve(testsDir, "../dist/Cli.mjs");
const fixture = resolve(testsDir, "multi-schema");
const publicSource = resolve(fixture, "src/public/Public.res");
const publicSourceUri = pathToFileURL(publicSource).href;
const child = spawn(process.execPath, [cli, "lsp", fixture], {stdio: ["pipe", "pipe", "pipe"]});
let stdout = "";
let stderr = "";
child.stdout.on("data", chunk => { stdout += chunk.toString(); });
child.stderr.on("data", chunk => { stderr += chunk.toString(); });

const send = message => {
  const payload = JSON.stringify(message);
  child.stdin.write(`Content-Length: ${Buffer.byteLength(payload)}\r\n\r\n${payload}`);
};

send({jsonrpc: "2.0", id: "initialize", method: "initialize", params: {}});
setTimeout(() => {
  send({
    jsonrpc: "2.0",
    method: "textDocument/didOpen",
    params: {textDocument: {uri: publicSourceUri, text: readFileSync(publicSource, "utf8")}},
  });
  send({
    jsonrpc: "2.0",
    id: "completion",
    method: "textDocument/completion",
    params: {textDocument: {uri: publicSourceUri}, position: {line: 3, character: 3}},
  });
}, 250);
setTimeout(() => send({jsonrpc: "2.0", id: "shutdown", method: "shutdown", params: null}), 750);
setTimeout(() => send({jsonrpc: "2.0", method: "exit", params: null}), 1000);

const timeout = setTimeout(() => {
  child.kill("SIGKILL");
  console.error("Multi-schema LSP did not exit in time.");
  process.exitCode = 1;
}, 7000);

child.on("close", code => {
  clearTimeout(timeout);
  const valid =
    code === 0 &&
    stdout.includes('"id":"initialize"') &&
    stdout.includes('"capabilities"') &&
    stdout.includes('"id":"completion"') &&
    stdout.includes("PublicContext.context");
  if (!valid) {
    console.error({code, stdout, stderr});
    process.exitCode = 1;
    return;
  }
  console.log("multi-schema LSP completion passed");
});
