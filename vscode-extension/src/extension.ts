import { ExtensionContext, workspace, OutputChannel, window } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import { cosmiconfig } from "cosmiconfig";
import { dirname, resolve } from "path";

export async function activate(context: ExtensionContext) {
  let currentWorkspacePath = workspace.workspaceFolders?.[0].uri.fsPath;
  if (currentWorkspacePath == null) throw new Error("Init failed.");

  let c = await cosmiconfig("resgraph", {
    searchPlaces: ["resgraph.json"],
  }).search(currentWorkspacePath);
  if (c == null) throw new Error("Could not find config.");

  let { filepath } = c;
  let fileDir = dirname(filepath);

  let serverOptions: ServerOptions = {
    transport: TransportKind.stdio,
    command: "npx",
    args: ["resgraph", "lsp", fileDir],
    options: {
      cwd: fileDir,
    },
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "rescript" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.res"),
    },
    outputChannelName: "ResGraph Language Server",
    revealOutputChannelOn: RevealOutputChannelOn.Never,
  };

  const client = new LanguageClient(
    "resgraph",
    "ResGraph Language Server",
    serverOptions,
    clientOptions
  );

  const disposable = client.start();
  context.subscriptions.push(disposable);
}

export function deactivate() {}
