@@directive("#!/usr/bin/env node")

@val
external argv: array<option<string>> = "process.argv"

module Console = Stdlib.Console
module JsExn = Js.Exn

let args = argv->Array.slice(~start=2)->Array.keepSome
let argsList = args->List.fromArray

let printBuildTime = buildDuration => {
  Console.log(`Build succeeded in ${(buildDuration /. 1000.)->Float.toFixed(~digits=2)} seconds.`)
}

let helpText = `
**ResGraph v0.1.0 CLI**
This is the CLI of ResGraph. All available configuration is made via adding a \`resgraph.json\` file in the root of your project.
Available commands:

init       | Initializes a new project.
build      | Builds the project.
watch      | Builds the project and watches for changes.
tools      | Show available ResGraph tools.
help       | Show this help message.
`

let toolsHelpText = `
Tools commands:

find-definition <TypeName[.fieldName]> [--json]

# Useful for finding the location of where a type or field is defined in the source code.
# By default, the output is a human-readable string. 
# With the \`--json\` flag, the output is a JSON string with the file path and location of the definition.
`

let parseFindDefinitionArgs = args =>
  switch args {
  | list{target} => Some((target, false))
  | list{target, "--json"} => Some((target, true))
  | list{"--json", target} => Some((target, true))
  | _ => None
  }

let validateConfig = config => {
  let issues = []
  config->InitProject.validateConfig(~issues)

  if issues->Array.length > 0 {
    issues->InitProject.printProjectIssues
    Process.process->Process.exitWithCode(1)
  }
}

let printFindDefinition = (~target, ~jsonOutput) => {
  let config = switch Utils.readConfigFromCwd() {
  | Error(msg) => panic(msg)
  | Ok(config) => config
  }

  validateConfig(config)

  switch Utils.callPrivateCli(FindDefinition({filePath: config.src, definitionHint: target})) {
  | FindDefinition({item: Some(item), error: None}) =>
    if jsonOutput {
      Console.log(item->Utils.stringifyFindDefinitionJson)
    } else {
      Console.log(item->Utils.formatFindDefinitionText)
    }
  | FindDefinition({item: None, error: Some(error)}) =>
    if jsonOutput {
      Console.log(Utils.stringifyFindDefinitionError(error))
    } else {
      Console.error(error)
    }
    Process.process->Process.exitWithCode(1)
  | _ =>
    Console.error("Unexpected response from ResGraph tools command.")
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
      GenerateSchema({
        src: config.src,
        outputFolder: config.outputFolder,
        dumpSchemaSdl: config.dumpSchemaSdl,
      }),
    )
    switch res {
    | Completion(_) | Hover(_) | Definition(_) | FindDefinition(_) | NotInitialized => ()
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
  | list{"tools", "find-definition", ...rest} =>
    switch parseFindDefinitionArgs(rest) {
    | Some((target, jsonOutput)) => printFindDefinition(~target, ~jsonOutput)
    | None =>
      Console.error("Invalid tools arguments.")
      Console.log(toolsHelpText)
      Process.process->Process.exitWithCode(1)
    }
  | list{"help"} => Console.log(helpText)
  | v =>
    Console.log("Invalid command: " ++ v->List.toArray->Array.join(" "))
    Console.log(helpText)
  }
} catch {
| Exn.Error(_) => Console.error("Error")
| _ => Console.error("Error!")
}
