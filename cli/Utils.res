// Some ad hoc simple bindings to URL
type url = {pathname: string}

@module("url") @new external makeUrl: (string, string) => url = "URL"

@val external currentFileUrl: string = "import.meta.url"
// End URL bindings

type config = {
  src: string,
  outputFolder: string,
  dumpSchemaSdl: bool,
}

let resolveRelative = path => Path.resolve([Process.process->Process.cwd, path])

type privateCliCall =
  | GenerateSchema({src: string, outputFolder: string, dumpSchemaSdl?: bool})
  | Completion({filePath: string, position: LspProtocol.loc, tmpname: string})
  | Hover({filePath: string, position: LspProtocol.loc})
  | HoverGraphQL({filePath: string, hoverHint: string})
  | Definition({filePath: string, definitionHint: string})
  | FindDefinition({filePath: string, definitionHint: string})

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
  | Definition({filePath, definitionHint}) => ["definition-graphql", filePath, definitionHint]
  | FindDefinition({filePath, definitionHint}) => ["find-definition", filePath, definitionHint]
  }

type analyzePosition = {
  line: int,
  column: int,
}

type analyzeRange = {
  start: analyzePosition,
  @as("end") end_: analyzePosition,
}

type findDefinitionItem = {
  path: string,
  kind: string,
  file: string,
  range: analyzeRange,
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
  | Definition({item: LspProtocol.definition})
  | FindDefinition({item: option<findDefinitionItem>, error: option<string>})

external toCallResult: string => callResult = "JSON.parse"

external infinity: int = "Infinity"

let devBinLocation = "../bin/dev/resgraph.exe"

let hasDevBin = Lazy.make(() => (devBinLocation->makeUrl(currentFileUrl)).pathname->Fs.existsSync)

@module("node:os")
external arch: unit => string = "arch"

let callPrivateCli = command => {
  let hasDevBin = hasDevBin->Lazy.get

  let binLocation = if hasDevBin {
    devBinLocation
  } else {
    "../bin/" ++
    switch (Os.platform(), arch()) {
    | ("darwin", "arm64") => "darwinarm64"
    | (platform, _) => platform
    } ++ "/resgraph.exe"
  }

  (binLocation->makeUrl(currentFileUrl)).pathname
  ->ChildProcess.execFileSyncWith(command->privateCliCallToArgs, {maxBuffer: infinity})
  ->Buffer.toString
  ->toCallResult
}

let formatFindDefinitionText = (item: findDefinitionItem) => {
  let {path, kind, file, range: {start, end_}} = item

  `path: ${path}\nkind: ${kind}\nfile: ${file}\nrange: ${start.line->Int.toString}:${start.column->Int.toString}-${end_.line->Int.toString}:${end_.column->Int.toString}`
}

type findDefinitionJson = {
  path: string,
  kind: string,
  file: string,
  range: analyzeRange,
}

type findDefinitionErrorJson = {error: string}

let stringifyFindDefinitionJson = (item: findDefinitionItem) => {
  let payload: findDefinitionJson = {
    path: item.path,
    kind: item.kind,
    file: item.file,
    range: item.range,
  }

  payload->JSON.stringifyAny
}

let stringifyFindDefinitionError = (error: string) => {
  let payload: findDefinitionErrorJson = {error: error}
  payload->JSON.stringifyAny
}

let getLastBuiltFromCompilerLog = compilerLogPath => {
  let compilerLogContents = compilerLogPath->Fs.readFileSync->Buffer.toString->String.split(Os.eol)

  // The "Done" marker is on the second line from the bottom, if it exists.
  let statusLine = compilerLogContents[compilerLogContents->Array.length - 2]->Option.getOr("")

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

let runIfCompilerDone = (fn, ~compilerLogPath, ~lastCompletedBuild) => {
  try {
    switch getLastBuiltFromCompilerLog(compilerLogPath) {
    | None => ()
    | Some(buildMarker) =>
      switch lastCompletedBuild.contents {
      | Some(lastBuildMarker) when lastBuildMarker == buildMarker => ()
      | _ =>
        lastCompletedBuild := Some(buildMarker)
        fn()
      }
    }
  } catch {
  | _ => ()
  }
}

let setupWatcher = (~onResult, ~onStartRebuild, ~config) => {
  let {src, outputFolder, dumpSchemaSdl} = config
  let compilerLogPath = Path.resolve([Process.process->Process.cwd, "./lib/bs/.compiler.log"])
  let lastCompletedBuild = ref(None)
  open Bindings.Chokidar

  let generateSchema = () => {
    onStartRebuild()
    let res = callPrivateCli(GenerateSchema({src, outputFolder, dumpSchemaSdl}))
    onResult(res)
  }

  generateSchema->runIfCompilerDone(~compilerLogPath, ~lastCompletedBuild)

  watcher
  ->watch(compilerLogPath)
  ->Watcher.onChange(compilerLogPath => {
    generateSchema->runIfCompilerDone(~compilerLogPath, ~lastCompletedBuild)
  })
  ->Watcher.onUnlink(compilerLogPath => {
    generateSchema->runIfCompilerDone(~compilerLogPath, ~lastCompletedBuild)
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
    let dumpSchemaSdl =
      switch dict->Dict.get("dumpSchemaSdl") {
      | None => Some(false)
      | Some(value) => value->JSON.Decode.bool
      }

    switch (
      dict->Dict.get("src")->Option.flatMap(JSON.Decode.string),
      dict->Dict.get("outputFolder")->Option.flatMap(JSON.Decode.string),
      dumpSchemaSdl,
    ) {
    | (Some(src), Some(outputFolder), Some(dumpSchemaSdl)) =>
      Some({
        src: src->resolveRelative,
        outputFolder: outputFolder->resolveRelative,
        dumpSchemaSdl,
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
  ->Buffer.toStringWithEncoding(StringEncoding.utf8)
  ->JSON.parseOrThrow
  ->parseConfig

  let res: result<config, string> = switch readConfigResult {
  | None => Error("Could not parse config, something is wrong")
  | Some(config) => Ok(config)
  }

  res
}

let readConfigFromCwd = () => readConfigFromDir(Process.process->Process.cwd)
