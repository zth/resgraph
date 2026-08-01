import {
  ExtensionContext,
  workspace,
  window,
  commands,
  Range,
  Position,
  Uri,
} from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import { cosmiconfig } from "cosmiconfig";
import { dirname, resolve } from "path";

const DEBUG = false;
function resolveWorkspaceCli(workspacePath: string): string {
  try {
    return require.resolve("resgraph/dist/Cli.mjs", {
      paths: [workspacePath],
    });
  } catch {
    throw new Error(
      "Could not find ResGraph in this workspace. Install resgraph in the project before starting the extension."
    );
  }
}


export async function activate(context: ExtensionContext) {
  let currentWorkspacePath = workspace.workspaceFolders?.[0].uri.fsPath;
  if (currentWorkspacePath == null) throw new Error("Init failed.");

  let c = await cosmiconfig("resgraph", {
    searchPlaces: ["resgraph.json"],
  }).search(currentWorkspacePath);
  if (c == null) throw new Error("Could not find config.");

  let { filepath } = c;
  let fileDir = dirname(filepath);
  const cliPath = DEBUG
    ? resolve(__filename, "../../../cli/Cli.mjs")
    : resolveWorkspaceCli(fileDir);

  if (DEBUG) {
    window.showInformationMessage("Running in debug mode." + __filename);
  }

  let serverOptions: ServerOptions = {
    transport: TransportKind.stdio,
    command: process.execPath,
    args: [cliPath, "lsp", fileDir],
    options: {
      cwd: fileDir,
    },
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "rescript" },
      { scheme: "file", language: "graphql" },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.{res,resi,graphql}"),
    },
    outputChannelName: "ResGraph Language Server",
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    markdown: {
      isTrusted: true,
    },
  };

  const client = new LanguageClient(
    "resgraph",
    "ResGraph Language Server",
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(
    commands.registerCommand(
      "vscode-resgraph.go_to_location",
      async (fileUri: string, startLine: number, startCol: number) => {
        await window.showTextDocument(Uri.parse(fileUri), {
          selection: new Range(
            new Position(startLine, startCol),
            new Position(startLine, startCol)
          ),
        });
      }
    )
  );

  const disposable = client.start();
  context.subscriptions.push(disposable);
}

export function deactivate() {}
