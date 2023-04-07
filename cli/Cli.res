@val
external argv: array<option<string>> = "process.argv"

let args = argv->Array.sliceToEnd(~start=2)->Array.keepSome
let argsList = args->List.fromArray

let resolveRelative = path => Path.resolve([Process.process->Process.cwd, path])

type privateCliCall =
  GenerateSchema({path: string, schemaOutputPath: string, assetsOutputPath: string})

let privateCliCallToArgs = call =>
  switch call {
  | GenerateSchema({path, schemaOutputPath, assetsOutputPath}) => [
      "generate-schema",
      path->resolveRelative,
      schemaOutputPath->resolveRelative,
      assetsOutputPath->resolveRelative,
    ]
  }

type generateError = {
  file: string,
  message: string,
  range: LspProtocol.range,
}

@tag("status") type callResult = Success | Error({errors: array<generateError>})

external toCallResult: string => callResult = "JSON.parse"

external infinity: int = "Infinity"

let callPrivateCli = command => {
  "../resgraph.exe"
  ->ChildProcess.execFileSyncWith(
    command->privateCliCallToArgs,
    ChildProcess.execFileSyncOptions(~maxBuffer=infinity, ()),
  )
  ->Buffer.toString
  ->toCallResult
}

let getLastBuiltFromCompilerLog = () => {
  let compilerLogConents =
    Fs.readFileSync(Path.resolve([Process.process->Process.cwd, ".compiler.log"]))
    ->Node.Buffer.toString
    ->String.split(Os.eol)

  // The "Done" marker is on the second line from the bottom, if it exists.
  let statusLine =
    compilerLogConents[compilerLogConents->Array.length - 2]->Option.getWithDefault("")

  if statusLine->String.startsWith("#Done(") {
    statusLine
    ->String.split("#Done(")
    ->Array.getUnsafe(1)
    ->String.split(")")
    ->Array.getUnsafe(0)
    ->Float.fromString
  } else {
    None
  }
}

try {
  switch argsList {
  | list{"watch", path, schemaOutputPath, assetsOutputPath} =>
    open Bindings.Chokidar

    let generateSchema = () => {
      let res = callPrivateCli(GenerateSchema({path, schemaOutputPath, assetsOutputPath}))
      Console.log(res)
    }

    let _theWatcher =
      watcher
      ->watch(Path.resolve([Process.process->Process.cwd, "./lib/bs/.compiler.log"]))
      ->Watcher.onChange(_ => {
        Console.log("Running because onChange")
        generateSchema()
      })
      ->Watcher.onUnlink(_ => {
        Console.log("Running because unlink")
        generateSchema()
      })

  | _ => Console.error("Invalid command.")
  }
} catch {
| _ => Console.log("Error!")
}
