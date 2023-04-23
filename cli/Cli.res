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

try {
  switch argsList {
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
  | v => Console.error("Invalid command: " ++ v->List.toArray->Array.joinWith(" "))
  }
} catch {
| Exn.Error(e) => Console.error("Error: " ++ e->Exn.message->Option.getWithDefault("-"))
| _ => Console.error("Error!")
}
