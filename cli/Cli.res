@@directive("#!/usr/bin/env node")

@val
external argv: array<option<string>> = "process.argv"

let args = argv->Array.sliceToEnd(~start=2)->Array.keepSome
let argsList = args->List.fromArray

let printBuildTime = buildDuration => {
  Console.log(
    `Build succeeded in ${(buildDuration /. 1000.)
        ->Float.toFixedWithPrecision(~digits=2)} seconds.`,
  )
}

let helpText = `
**ResGraph v0.1.0 CLI**
This is the CLI of ResGraph. All available configuration is made via adding a \`resgraph.json\` file in the root of your project.
Available commands:

init       | Initializes a new project.
build      | Builds the project.
watch      | Builds the project and watches for changes.
help       | Show this help message.
`

try {
  switch argsList {
  | list{"init"} =>
    let projectDir = Process.process->Process.cwd

    Console.log("Initializing ResGraph project...\n")

    let issues = InitProject.validateProject(projectDir)

    if issues->Array.length > 0 {
      Console.error("⛔ One or more issues was encountered with this ResGraph project.\n")
      issues->Array.forEach(issue =>
        switch issue {
        | MissingConfigFile =>
          Console.error(
            `- 🚫 The file "resgraph.json" does not exist in this project root (${projectDir}). Please create it and add + configure the content below to your liking:
{
  "src": "./src",
  "outputFolder": "./src/__generated__"
}`,
          )
        | MissingContextFile => /* TODO: Ask priv bin for whether assets exist */ ()
        | OutputFolderDoesNotExist({path}) =>
          Console.error(
            `- 🚫 "outputFolder" in "resgraph.json" is configured to be "${path}", but that folder either does not exist, or is not possible to access.`,
          )
        | ConfigFileIssue =>
          /* TODO: Link to docs */
          Console.error(`- 🚫 "resgraph.json" exists but contains issues. Please double check it's configured correctly.`)
        | SrcFolderDoesNotExist({path}) =>
          Console.error(
            `- 🚫 "src" in "resgraph.json" is configured to be "${path}", but that folder either does not exist, or is not possible to access.`,
          )
        }
      )

      Process.process->Process.exitWithCode(1)
    } else {
      Console.log("✅ Project already set up correctly.")
      Process.process->Process.exitWithCode(0)
    }

  | list{"build"} =>
    let config = switch Utils.readConfigFromCwd() {
    | Error(msg) => panic(msg)
    | Ok(config) => config
    }

    open PerfHooks.Performance
    let timeStart = performance->now

    let res = Utils.callPrivateCli(
      GenerateSchema({src: config.src, outputFolder: config.outputFolder}),
    )
    switch res {
    | Completion(_) | NotInitialized => ()
    | Success(_) =>
      let buildDuration = performance->now -. timeStart
      printBuildTime(buildDuration)
    | Error({errors}) =>
      let fileContentCache = Dict.make()

      errors->Array.forEach(error => {
        let fileContentLines = switch fileContentCache->Dict.get(error.file) {
        | Some(content) => content
        | None =>
          let contents =
            error.file
            ->Fs.readFileSync
            ->Node.Buffer.toStringWithEncoding(#utf8)
            ->String.split(Os.eol)
          fileContentCache->Dict.set(error.file, contents)
          contents
        }

        ErrorPrinter.prettyPrintDiagnostic(~lines=fileContentLines, ~diagnostic=error)
      })

      Process.process->Process.exitWithCode(1)
    }
  | list{"watch"} =>
    let config = switch Utils.readConfigFromCwd() {
    | Error(msg) => panic(msg)
    | Ok(config) => config
    }

    open PerfHooks.Performance
    let timeStart = ref(0.)

    let _watcher = Utils.setupWatcher(
      ~onResult=_ => {
        let buildDuration = performance->now -. timeStart.contents
        printBuildTime(buildDuration)
      },
      ~onStartRebuild=() => {
        Console.clear()
        Console.log("Rebuilding.")
        timeStart := performance->now
      },
      ~config,
    )
    Console.log("Watching for changes...")
  | list{"lsp", configFilePath} => Lsp.start(~configFilePath, ~mode=Lsp.Stdio)
  | list{"help"} => Console.log(helpText)
  | v =>
    Console.log("Invalid command: " ++ v->List.toArray->Array.joinWith(" "))
    Console.log(helpText)
  }
} catch {
| Exn.Error(e) => Console.error("Error: " ++ e->Exn.message->Option.getWithDefault("-"))
| _ => Console.error("Error!")
}
