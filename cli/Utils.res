let resolveRelative = path => Path.resolve([Process.process->Process.cwd, path])

type privateCliCall =
  | GenerateSchema({path: string, schemaOutputPath: string, assetsOutputPath: string})
  | Completion({filePath: string, position: LspProtocol.loc, tmpname: string})

let privateCliCallToArgs = call =>
  switch call {
  | GenerateSchema({path, schemaOutputPath, assetsOutputPath}) => [
      "generate-schema",
      path->resolveRelative,
      schemaOutputPath->resolveRelative,
      assetsOutputPath->resolveRelative,
    ]
  | Completion({filePath, position, tmpname}) => [
      "completion",
      filePath,
      position.line->Int.toString,
      position.character->Int.toString,
      tmpname,
    ]
  }

type generateError = {
  file: string,
  message: string,
  range: LspProtocol.range,
}

@tag("status")
type callResult =
  | NotInitialized
  | Success({ok: bool})
  | Error({errors: array<generateError>})
  | Completion({items: array<LspProtocol.completionItem>})

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

let getLastBuiltFromCompilerLog = compilerLogPath => {
  let compilerLogContents =
    compilerLogPath->Fs.readFileSync->Node.Buffer.toString->String.split(Os.eol)

  // The "Done" marker is on the second line from the bottom, if it exists.
  let statusLine =
    compilerLogContents[compilerLogContents->Array.length - 2]->Option.getWithDefault("")

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

let runIfCompilerDone = (fn, ~compilerLogPath) => {
  try {
    switch getLastBuiltFromCompilerLog(compilerLogPath) {
    | None => ()
    | Some(_) => fn()
    }
  } catch {
  | _ => ()
  }
}

let setupWatcher = (~onResult, ~path, ~schemaOutputPath, ~assetsOutputPath) => {
  open Bindings.Chokidar

  let generateSchema = () => {
    let res = callPrivateCli(GenerateSchema({path, schemaOutputPath, assetsOutputPath}))
    onResult(res)
  }

  watcher
  ->watch(Path.resolve([Process.process->Process.cwd, "./lib/bs/.compiler.log"]))
  ->Watcher.onChange(compilerLogPath => {
    generateSchema->runIfCompilerDone(~compilerLogPath)
  })
  ->Watcher.onUnlink(compilerLogPath => {
    generateSchema->runIfCompilerDone(~compilerLogPath)
  })
}

let tempFilePrefix = "resgraph_support_file_" ++ Process.process->Process.pid->Int.toString ++ "_"
let tempFileId = ref(0)

let createFileInTempDir = (~extension="") => {
  let tempFileName = tempFilePrefix ++ tempFileId.contents->Int.toString ++ extension
  tempFileId := tempFileId.contents + 1
  Path.join([Os.tmpdir(), tempFileName])
}
