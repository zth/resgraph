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

let validateConfig = config => {
  let issues = []
  config->InitProject.validateConfig(~issues)

  if issues->Array.length > 0 {
    issues->InitProject.printProjectIssues
    Process.process->Process.exitWithCode(1)
  }
}

try {
  switch argsList {
  | list{"init"} =>
    let projectDir = Process.process->Process.cwd

    Console.log("Initializing ResGraph project...\n")

    let issues = InitProject.validateProject(projectDir)

    if issues->Array.length > 0 {
      issues->InitProject.printProjectIssues
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

    validateConfig(config)

    open PerfHooks.Performance
    let timeStart = performance->now

    let res = Utils.callPrivateCli(
      GenerateSchema({src: config.src, outputFolder: config.outputFolder, dumpSchemaSdl: true}),
    )
    switch res {
    | Completion(_) | Hover(_) | Definition(_) | NotInitialized => ()
    | Success(_) =>
      let buildDuration = performance->now -. timeStart
      printBuildTime(buildDuration)
    | Error({errors}) =>
      ErrorPrinter.printErrors(errors)
      Process.process->Process.exitWithCode(1)
    }
  | list{"watch"} =>
    let config = switch Utils.readConfigFromCwd() {
    | Error(msg) => panic(msg)
    | Ok(config) => config
    }

    validateConfig(config)

    open PerfHooks.Performance
    let timeStart = ref(0.)

    let _watcher = Utils.setupWatcher(
      ~onResult=res => {
        let buildDuration = performance->now -. timeStart.contents
        printBuildTime(buildDuration)

        switch res {
        | Error({errors}) => ErrorPrinter.printErrors(errors)
        | _ => ()
        }
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
