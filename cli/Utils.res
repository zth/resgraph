// Some ad hoc simple bindings to URL
type url = {pathname: string}

@module("url") @new external makeUrl: (string, string) => url = "URL"

@val external currentFileUrl: string = "import.meta.url"
// End URL bindings

type config = {
  src: string,
  outputFolder: string,
}

let resolveRelative = path => Path.resolve([Process.process->Process.cwd, path])

type privateCliCall =
  | GenerateSchema({src: string, outputFolder: string, dumpSchemaSdl?: bool})
  | Completion({filePath: string, position: LspProtocol.loc, tmpname: string})
  | Hover({filePath: string, position: LspProtocol.loc})
  | HoverGraphQL({filePath: string, hoverHint: string})

let privateCliCallToArgs = call =>
  switch call {
  | GenerateSchema({src, outputFolder, ?dumpSchemaSdl}) =>
    [
      "generate-schema",
      src->resolveRelative,
      outputFolder->resolveRelative,
      switch dumpSchemaSdl {
      | Some(true) => "true"
      | Some(false) | None => ""
      },
    ]->Array.filter(s => s != "")
  | Completion({filePath, position, tmpname}) => [
      "completion",
      filePath,
      position.line->Int.toString,
      position.character->Int.toString,
      tmpname,
    ]
  | Hover({filePath, position}) => [
      "hover",
      filePath,
      position.line->Int.toString,
      position.character->Int.toString,
    ]
  | HoverGraphQL({filePath, hoverHint}) => ["hover-graphql", filePath, hoverHint]
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
  | Hover({item: LspProtocol.hover})

external toCallResult: string => callResult = "JSON.parse"

external infinity: int = "Infinity"

let devBinLocation = "../bin/dev/resgraph.exe"

let hasDevBin = Lazy.from_fun(() =>
  (devBinLocation->makeUrl(currentFileUrl)).pathname->Fs.existsSync
)

let callPrivateCli = command => {
  let hasDevBin = hasDevBin->Lazy.force

  // TODO: macos-arm
  let binLocation = if hasDevBin {
    devBinLocation
  } else {
    "../bin/" ++ Os.platform() ++ "/resgraph.exe"
  }

  (binLocation->makeUrl(currentFileUrl)).pathname
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

let setupWatcher = (~onResult, ~onStartRebuild, ~config) => {
  let {src, outputFolder} = config
  open Bindings.Chokidar

  let generateSchema = () => {
    onStartRebuild()
    let res = callPrivateCli(GenerateSchema({src, outputFolder, dumpSchemaSdl: true}))
    onResult(res)
  }

  // Build on watcher start
  generateSchema()

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

let parseConfig = rawConfig => {
  switch rawConfig->JSON.Decode.object {
  | None => None
  | Some(dict) =>
    switch (
      dict->Dict.get("src")->Option.flatMap(JSON.Decode.string),
      dict->Dict.get("outputFolder")->Option.flatMap(JSON.Decode.string),
    ) {
    | (Some(src), Some(outputFolder)) =>
      Some({
        src: src->resolveRelative,
        outputFolder: outputFolder->resolveRelative,
      })
    | _ => None
    }
  }
}

let readConfigFromDir = dir => {
  let readConfigResult =
    [dir, "./resgraph.json"]
    ->Path.resolve
    ->Fs.readFileSync
    ->Node.Buffer.toStringWithEncoding(#utf8)
    ->JSON.parseExn
    ->parseConfig

  switch readConfigResult {
  | None => Result.Error("Could not parse config, something is wrong")
  | Some(config) => Ok(config)
  }
}

let readConfigFromCwd = () => readConfigFromDir(Process.process->Process.cwd)
