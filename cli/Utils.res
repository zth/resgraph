// Some ad hoc simple bindings to URL
type url = {pathname: string}

@module("url") @new external makeUrl: (string, string) => url = "URL"

@val external currentFileUrl: string = "import.meta.url"
@module("node:fs") external realpathSync: string => string = "realpathSync"
// End URL bindings

let isWindows: bool = %raw(`process.platform === "win32"`)

let canonicalPath = path => {
  let canonical = try {
    realpathSync(path)
  } catch {
  | _ => path
  }
  if isWindows {
    canonical->String.toLowerCase
  } else {
    canonical
  }
}

let portableFilesystemIdentity = path => path->canonicalPath->String.toLowerCase

type authorizationMode = Required | Baseline

type authorizationConfig = {
  mode: authorizationMode,
  onForbidden?: string,
  manifestPath?: string,
  baselinePath?: string,
}

type schemaConfig = {
  name: string,
  projectRoot: string,
  includePaths: array<string>,
  excludePaths: array<string>,
  outputFolder: string,
  moduleName: string,
  contextType: string,
  dumpSchemaSdl: bool,
  authorization?: authorizationConfig,
  stateName: option<string>,
}

type config = {
  schemas: array<schemaConfig>,
  defaultSchema: string,
  legacy: bool,
  src: string,
  outputFolder: string,
  dumpSchemaSdl: bool,
  authorization?: authorizationConfig,
}

let resolveRelative = (path, ~baseDir) => Path.resolve([baseDir, path])

type privateCliCall =
  | GenerateSchema(schemaConfig)
  | Completion({filePath: string, position: LspProtocol.loc, tmpname: string, stateName?: string})
  | Hover({filePath: string, position: LspProtocol.loc, stateName?: string})
  | HoverGraphQL({filePath: string, hoverHint: string, stateName?: string})
  | Definition({filePath: string, definitionHint: string, stateName?: string})
  | FindDefinition({filePath: string, definitionHint: string, stateName?: string})

let optionStringToArray = value =>
  switch value {
  | None => []
  | Some(value) => [value]
  }

let privateCliCallToArgs = call =>
  switch call {
  | GenerateSchema({
      projectRoot,
      includePaths,
      excludePaths,
      outputFolder,
      moduleName,
      contextType,
      dumpSchemaSdl,
      ?authorization,
      stateName,
    }) =>
    let baseArgs = [
      "generate-schema",
      projectRoot,
      outputFolder,
      if dumpSchemaSdl {
        "true"
      } else {
        "false"
      },
    ]
    let authorizationArgs = switch authorization {
    | None => []
    | Some(authorization) => [
        switch authorization.mode {
        | Required => "required"
        | Baseline => "baseline"
        },
        authorization.onForbidden->Option.getOr("-"),
        authorization.manifestPath->Option.getOr("-"),
        authorization.baselinePath->Option.getOr("-"),
      ]
    }
    let args = baseArgs->Array.concat(authorizationArgs)
    switch stateName {
    | None => args
    | Some(stateName) =>
      args
      ->Array.concat(["--schema", stateName, "--module", moduleName, "--context", contextType])
      ->Array.concat(includePaths->Array.flatMap(path => ["--include", path]))
      ->Array.concat(
        excludePaths->Array.concat([outputFolder])->Array.flatMap(path => ["--exclude", path]),
      )
    }
  | Completion({filePath, position, tmpname, ?stateName}) =>
    [
      "completion",
      filePath,
      position.line->Int.toString,
      position.character->Int.toString,
      tmpname,
      ...stateName->optionStringToArray,
    ]
  | Hover({filePath, position, ?stateName}) =>
    [
      "hover",
      filePath,
      position.line->Int.toString,
      position.character->Int.toString,
      ...stateName->optionStringToArray,
    ]
  | HoverGraphQL({filePath, hoverHint, ?stateName}) =>
    ["hover-graphql", filePath, hoverHint, ...stateName->optionStringToArray]
  | Definition({filePath, definitionHint, ?stateName}) =>
    ["definition-graphql", filePath, definitionHint, ...stateName->optionStringToArray]
  | FindDefinition({filePath, definitionHint, ?stateName}) =>
    ["find-definition", filePath, definitionHint, ...stateName->optionStringToArray]
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
      | Some(lastBuildMarker) if lastBuildMarker === buildMarker => ()
      | _ =>
        lastCompletedBuild := Some(buildMarker)
        fn()
      }
    }
  } catch {
  | _ => ()
  }
}

type watcherResult = GeneratorResult(callResult) | GeneratorProcessFailure

let hasProjectConfig = root =>
  Fs.existsSync(Path.join([root, "rescript.json"])) ||
  Fs.existsSync(Path.join([root, "bsconfig.json"]))

let rec findCompilerRoot = path =>
  if path->hasProjectConfig {
    Some(path)
  } else {
    let parent = Path.dirname(path)
    if parent === path {
      None
    } else {
      parent->findCompilerRoot
    }
  }

let setupWatcher = (~onResult, ~onStartRebuild, ~config: schemaConfig) => {
  let compilerRoot = config.projectRoot->findCompilerRoot->Option.getOr(config.projectRoot)
  let compilerLogPath = Path.resolve([compilerRoot, "./lib/bs/.compiler.log"])
  let lastCompletedBuild = ref(None)
  open Bindings.Chokidar

  let generateSchema = () => {
    onStartRebuild(config)
    try {
      let res = callPrivateCli(GenerateSchema(config))
      onResult(config, GeneratorResult(res))
    } catch {
    | Exn.Error(error) =>
      Stdlib.Console.error(`[${config.name}] Generator process failed.`)
      Stdlib.Console.error(error)
      onResult(config, GeneratorProcessFailure)
    | _ =>
      Stdlib.Console.error(`[${config.name}] Generator process failed.`)
      onResult(config, GeneratorProcessFailure)
    }
  }

  lastCompletedBuild := try {
    getLastBuiltFromCompilerLog(compilerLogPath)
  } catch {
  | _ => None
  }

  let compilerWatcher =
    watcher
    ->watch(compilerLogPath)
    ->Watcher.onChange(compilerLogPath => {
      generateSchema->runIfCompilerDone(~compilerLogPath, ~lastCompletedBuild)
    })
    ->Watcher.onUnlink(compilerLogPath => {
      generateSchema->runIfCompilerDone(~compilerLogPath, ~lastCompletedBuild)
    })

  generateSchema()
  generateSchema->runIfCompilerDone(~compilerLogPath, ~lastCompletedBuild)
  compilerWatcher
}

let tempFilePrefix = "resgraph_support_file_" ++ Process.process->Process.pid->Int.toString ++ "_"
let tempFileId = ref(0)

let createFileInTempDir = (~extension="") => {
  let tempFileName = tempFilePrefix ++ tempFileId.contents->Int.toString ++ extension
  tempFileId := tempFileId.contents + 1
  Path.join([Os.tmpdir(), tempFileName])
}

let parseOptionalString = (dict, key) =>
  switch dict->Dict.get(key) {
  | None => Some(None)
  | Some(value) => value->JSON.Decode.string->Option.map(value => Some(value))
  }

let parseAuthorizationConfig = (dict, ~baseDir) =>
  switch dict->Dict.get("authorization") {
  | None => Some(None)
  | Some(value) =>
    value
    ->JSON.Decode.object
    ->Option.flatMap(authorization =>
      switch (
        authorization->Dict.get("mode")->Option.flatMap(JSON.Decode.string),
        parseOptionalString(authorization, "onForbidden"),
        parseOptionalString(authorization, "manifestPath"),
        parseOptionalString(authorization, "baselinePath"),
      ) {
      | (Some("required"), Some(onForbidden), Some(manifestPath), Some(baselinePath)) =>
        Some(
          Some({
            mode: Required,
            ?onForbidden,
            manifestPath: ?(manifestPath->Option.map(resolveRelative(~baseDir, ...))),
            baselinePath: ?(baselinePath->Option.map(resolveRelative(~baseDir, ...))),
          }),
        )
      | _ => None
      }
    )
  }

let decodeStringArray = (dict, key, ~default) =>
  switch dict->Dict.get(key) {
  | None => Some(default)
  | Some(value) =>
    value
    ->JSON.Decode.array
    ->Option.flatMap(values => {
      let decoded = values->Array.map(JSON.Decode.string)
      if decoded->Array.every(Option.isSome) {
        Some(decoded->Array.keepSome)
      } else {
        None
      }
    })
  }

let capitalizeFirst = value =>
  switch value->String.get(0) {
  | None => value
  | Some(first) => first->String.toUpperCase ++ value->String.slice(~start=1)
  }

let moduleNameFromSchemaName = name =>
  name
  ->String.split("-")
  ->Array.flatMap(part => part->String.split("_"))
  ->Array.map(capitalizeFirst)
  ->Array.join("") ++ "Schema"

let decodeSchema = (~name, ~baseDir, ~projectRoot, dict, ~stateName) => {
  let dumpSchemaSdl = switch dict->Dict.get("dumpSchemaSdl") {
  | None => Some(false)
  | Some(value) => value->JSON.Decode.bool
  }
  let contextType = switch dict->Dict.get("contextType") {
  | None => Some("ResGraphContext.context")
  | Some(value) => value->JSON.Decode.string
  }
  let moduleName = switch dict->Dict.get("moduleName") {
  | None => Some(name->moduleNameFromSchemaName)
  | Some(value) => value->JSON.Decode.string
  }

  switch (
    dict->Dict.get("outputFolder")->Option.flatMap(JSON.Decode.string),
    decodeStringArray(dict, "include", ~default=[]),
    decodeStringArray(dict, "exclude", ~default=[]),
    moduleName,
    contextType,
    dumpSchemaSdl,
    parseAuthorizationConfig(dict, ~baseDir),
  ) {
  | (
      Some(outputFolder),
      Some(includePaths),
      Some(excludePaths),
      Some(moduleName),
      Some(contextType),
      Some(dumpSchemaSdl),
      Some(authorization),
    ) =>
    Some({
      name,
      projectRoot: projectRoot->resolveRelative(~baseDir),
      includePaths: includePaths->Array.map(resolveRelative(~baseDir, ...)),
      excludePaths: excludePaths->Array.map(resolveRelative(~baseDir, ...)),
      outputFolder: outputFolder->resolveRelative(~baseDir),
      moduleName,
      contextType,
      dumpSchemaSdl,
      ?authorization,
      stateName,
    })
  | _ => None
  }
}

let parseConfig = (rawConfig, ~baseDir=Process.process->Process.cwd) => {
  switch rawConfig->JSON.Decode.object {
  | None => None
  | Some(dict) =>
    switch dict->Dict.get("schemas") {
    | Some(rawSchemas) =>
      switch rawSchemas->JSON.Decode.object {
      | None => None
      | Some(schemaDict) =>
        let schemas =
          schemaDict
          ->Dict.toArray
          ->Array.map(((name, rawSchema)) =>
            rawSchema
            ->JSON.Decode.object
            ->Option.flatMap(schemaDict => {
              let projectRoot = switch schemaDict->Dict.get("projectRoot") {
              | None => Some(".")
              | Some(value) => value->JSON.Decode.string
              }
              projectRoot->Option.flatMap(
                projectRoot =>
                  decodeSchema(~name, ~baseDir, ~projectRoot, schemaDict, ~stateName=Some(name)),
              )
            })
          )
        if schemas->Array.length === 0 || !(schemas->Array.every(Option.isSome)) {
          None
        } else {
          let schemas = schemas->Array.keepSome
          let defaultSchema = switch dict->Dict.get("defaultSchema") {
          | Some(value) => value->JSON.Decode.string
          | None => schemas->Array.get(0)->Option.map(schema => schema.name)
          }
          defaultSchema->Option.flatMap(defaultSchema =>
            schemas->Array.find(schema => schema.name === defaultSchema)->Option.map(schema => {
              let authorization = schema.authorization
              {
                schemas,
                defaultSchema,
                legacy: false,
                src: schema.projectRoot,
                outputFolder: schema.outputFolder,
                dumpSchemaSdl: schema.dumpSchemaSdl,
                ?authorization,
              }
            })
          )
        }
      }
    | None =>
      switch dict->Dict.get("src")->Option.flatMap(JSON.Decode.string) {
      | Some(src) =>
        decodeSchema(
          ~name="default",
          ~baseDir,
          ~projectRoot=src,
          dict,
          ~stateName=None,
        )->Option.map(schema => {
          let schema = {...schema, moduleName: "ResGraphSchema"}
          let authorization = schema.authorization
          {
            schemas: [schema],
            defaultSchema: "default",
            legacy: true,
            src: schema.projectRoot,
            outputFolder: schema.outputFolder,
            dumpSchemaSdl: schema.dumpSchemaSdl,
            ?authorization,
          }
        })
      | None => None
      }
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
    ->parseConfig(~baseDir=dir)

  let res: result<config, string> = switch readConfigResult {
  | None => Error("Could not parse config, something is wrong")
  | Some(config) => Ok(config)
  }

  res
}

let readConfigFromCwd = () => readConfigFromDir(Process.process->Process.cwd)

let findSchema = (config, name) => config.schemas->Array.find(schema => schema.name === name)

let defaultSchema = config => config->findSchema(config.defaultSchema)

let pathMatches = (path, root) => path === root || path->String.startsWith(root ++ Path.sep)

let schemaForFile = (config, filePath) => {
  let filePath = filePath->canonicalPath
  let matching = config.schemas->Array.filter(schema => {
    let projectRoot = schema.projectRoot->canonicalPath
    let includePaths = schema.includePaths->Array.map(canonicalPath)
    let excludePaths = schema.excludePaths->Array.map(canonicalPath)
    let included =
      filePath->pathMatches(projectRoot) &&
        (includePaths->Array.length === 0 ||
          includePaths->Array.some(root => filePath->pathMatches(root)))
    let excluded = excludePaths->Array.some(root => filePath->pathMatches(root))
    included && !excluded
  })

  switch matching {
  | [schema] => Some(schema)
  | _ => config->defaultSchema
  }
}

let schemaForGraphqlFile = (config, filePath) => {
  let filePath = filePath->canonicalPath
  let mostSpecificSchema = ref(None)
  let mostSpecificLength = ref(-1)

  config.schemas->Array.forEach(schema => {
    let outputFolder = schema.outputFolder->canonicalPath
    if (
      filePath->pathMatches(outputFolder) &&
        outputFolder->String.length > mostSpecificLength.contents
    ) {
      mostSpecificSchema := Some(schema)
      mostSpecificLength := outputFolder->String.length
    }
  })

  mostSpecificSchema.contents->Option.orElse(config->defaultSchema)
}
