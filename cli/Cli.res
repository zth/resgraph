@@directive("#!/usr/bin/env node")

@val
external argv: array<option<string>> = "process.argv"

module Console = Stdlib.Console
module JsExn = Js.Exn

open PerfHooks.Performance

let args = argv->Array.slice(~start=2)->Array.keepSome
let argsList = args->List.fromArray

let printBuildTime = (schema: Utils.schemaConfig, buildDuration, ~showSchemaName) => {
  let prefix = if showSchemaName {
    `[${schema.name}] `
  } else {
    ""
  }
  Console.log(
    `${prefix}Build succeeded in ${(buildDuration /. 1000.)->Float.toFixed(~digits=2)} seconds.`,
  )
}

let printAuthorizationBaselineWarning = (authorization: option<Utils.authorizationConfig>) =>
  switch authorization->Option.flatMap(authorization => authorization.baselinePath) {
  | None => ()
  | Some(baselinePath) =>
    Console.warn(
      `⚠️ Authorization baseline active: fields listed in ${baselinePath} are not protected by required authorization coverage.`,
    )
  }

let helpText = `
**ResGraph CLI**
This is the CLI of ResGraph. All configuration is read from \`resgraph.json\`.
Available commands:

init                            | Validate the project configuration.
build [schema]                  | Build all schemas, or one named schema.
authorization baseline [schema] | Create or update a schema's authorization baseline.
watch [schema]                  | Watch all schemas, or one named schema.
tools                           | Show available ResGraph tools.
help                            | Show this help message.
`

let toolsHelpText = `
Tools commands:

find-definition <TypeName[.fieldName]> [--schema <schema>] [--json]
`

type findDefinitionArgs = {target: string, jsonOutput: bool, schemaName: option<string>}
type findDefinitionParseState = {
  target: option<string>,
  jsonOutput: bool,
  schemaName: option<string>,
}

let parseFindDefinitionArgs = args => {
  let rec loop = (remaining, state: findDefinitionParseState) =>
    switch remaining {
    | list{} =>
      state.target->Option.map(target => {
        let result: findDefinitionArgs = {
          target,
          jsonOutput: state.jsonOutput,
          schemaName: state.schemaName,
        }
        result
      })
    | list{"--json", ...rest} if !state.jsonOutput => loop(rest, {...state, jsonOutput: true})
    | list{"--schema", schemaName, ...rest} if state.schemaName->Option.isNone =>
      loop(rest, {...state, schemaName: Some(schemaName)})
    | list{target, ...rest} if state.target->Option.isNone && !(target->String.startsWith("--")) =>
      loop(rest, {...state, target: Some(target)})
    | _ => None
    }

  loop(args, {target: None, jsonOutput: false, schemaName: None})
}

let readConfig = () =>
  switch Utils.readConfigFromCwd() {
  | Error(msg) => panic(msg)
  | Ok(config) => config
  }

let validateConfig = config => {
  let issues = []
  config->InitProject.validateConfig(~issues, ~configDir=Process.process->Process.cwd)

  if issues->Array.length > 0 {
    issues->InitProject.printProjectIssues
    Process.process->Process.exitWithCode(1)
  }
}

let selectSchemas = (config: Utils.config, schemaName) =>
  switch schemaName {
  | None => config.schemas
  | Some(schemaName) =>
    switch config->Utils.findSchema(schemaName) {
    | Some(schema) => [schema]
    | None =>
      Console.error(`Unknown ResGraph schema "${schemaName}".`)
      Process.process->Process.exitWithCode(1)
      []
    }
  }

let printFindDefinition = (~target, ~jsonOutput, ~schemaName) => {
  let config = readConfig()
  validateConfig(config)

  let schema = switch schemaName {
  | Some(schemaName) => config->Utils.findSchema(schemaName)
  | None => config->Utils.defaultSchema
  }

  switch schema {
  | None =>
    Console.error("Could not select a ResGraph schema.")
    Process.process->Process.exitWithCode(1)
  | Some(schema) =>
    let stateName = schema.stateName
    switch Utils.callPrivateCli(
      FindDefinition({filePath: schema.projectRoot, definitionHint: target, ?stateName}),
    ) {
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
}

let buildSchemas = (config: Utils.config, schemas: array<Utils.schemaConfig>) => {
  let showSchemaName = !config.legacy || schemas->Array.length > 1
  let hadError = ref(false)
  let schemasByCompilerRoot: Dict.t<array<Utils.schemaConfig>> = Dict.make()

  schemas->Array.forEach(schema => {
    let compilerRoot =
      schema.projectRoot->Utils.findCompilerRoot->Option.getOr(schema.projectRoot)->Utils.canonicalPath
    switch schemasByCompilerRoot->Dict.get(compilerRoot) {
    | Some(group) => group->Array.push(schema)
    | None => schemasByCompilerRoot->Dict.set(compilerRoot, [schema])
    }
  })

  schemasByCompilerRoot->Dict.toArray->Array.forEach(((_compilerRoot, group)) => {
    let timeStart = performance->now
    try {
      let results = Utils.callPrivateCliBatch(group)
      if results->Array.length !== group->Array.length {
        panic("Native batch response did not match the requested schema count.")
      }
      results->Array.forEachWithIndex((result, index) => {
        let schema = group[index]->Option.getOrThrow(~message="Missing schema for batch result.")
        switch result {
        | Completion(_) | Hover(_) | Definition(_) | FindDefinition(_) | NotInitialized => ()
        | Success(_) =>
          printBuildTime(schema, performance->now -. timeStart, ~showSchemaName)
          printAuthorizationBaselineWarning(schema.authorization)
        | Error({errors}) =>
          if showSchemaName {
            Console.error(`[${schema.name}] Schema generation failed.`)
          }
          ErrorPrinter.printErrors(errors)
          hadError := true
        }
      })
    } catch {
    | Exn.Error(error) =>
      group->Array.forEach(schema =>
        Console.error(`[${schema.name}] Generator process failed.`)
      )
      Console.error(error)
      hadError := true
    | _ =>
      group->Array.forEach(schema =>
        Console.error(`[${schema.name}] Generator process failed.`)
      )
      hadError := true
    }
  })

  if hadError.contents {
    Process.process->Process.exitWithCode(1)
  }
}

let generateAuthorizationBaseline = schemaName => {
  let config = readConfig()
  validateConfig(config)
  let schema = switch schemaName {
  | None => config->Utils.defaultSchema
  | Some(schemaName) => config->Utils.findSchema(schemaName)
  }
  let schema = schema->Option.getOrThrow(~message="Could not select a ResGraph schema.")

  switch schema.authorization {
  | None =>
    Console.error(
      `Required authorization must be configured for schema "${schema.name}" before creating a baseline.`,
    )
    Process.process->Process.exitWithCode(1)
  | Some(authorization) =>
    switch authorization.baselinePath {
    | None =>
      Console.error("Set `authorization.baselinePath` in resgraph.json before creating a baseline.")
      Process.process->Process.exitWithCode(1)
    | Some(baselinePath) =>
      let baselineAuthorization = {...authorization, mode: Baseline}
      let baselineSchema = {...schema, authorization: baselineAuthorization}
      GeneratedArtifacts.sync(
        config,
        ~selectedSchemas=[baselineSchema],
        ~configDir=Process.process->Process.cwd,
      )
      switch Utils.callPrivateCli(GenerateSchema(baselineSchema)) {
      | Success(_) =>
        Console.log(`Authorization baseline written to ${baselinePath}.`)
        printAuthorizationBaselineWarning(Some(baselineAuthorization))
      | Error({errors}) =>
        ErrorPrinter.printErrors(errors)
        Process.process->Process.exitWithCode(1)
      | _ =>
        Console.error("Unexpected response while creating the authorization baseline.")
        Process.process->Process.exitWithCode(1)
      }
    }
  }
}

try {
  switch argsList {
  | list{"init"} =>
    let issues = InitProject.validateProject(Process.process->Process.cwd)
    if issues->Array.length > 0 {
      issues->InitProject.printProjectIssues
      Process.process->Process.exitWithCode(1)
    } else {
      Console.log("✅ Project already set up correctly.")
    }
  | list{"authorization", "baseline"} => generateAuthorizationBaseline(None)
  | list{"authorization", "baseline", schemaName} => generateAuthorizationBaseline(Some(schemaName))
  | list{"build"} =>
    let config = readConfig()
    validateConfig(config)
    let schemas = config->selectSchemas(None)
    GeneratedArtifacts.sync(
      config,
      ~selectedSchemas=schemas,
      ~configDir=Process.process->Process.cwd,
    )
    buildSchemas(config, schemas)
  | list{"build", schemaName} =>
    let config = readConfig()
    validateConfig(config)
    let schemas = config->selectSchemas(Some(schemaName))
    GeneratedArtifacts.sync(
      config,
      ~selectedSchemas=schemas,
      ~configDir=Process.process->Process.cwd,
    )
    buildSchemas(config, schemas)
  | list{"watch"} | list{"watch", _} =>
    let config = readConfig()
    validateConfig(config)
    let schemaName = switch argsList {
    | list{"watch", schemaName} => Some(schemaName)
    | _ => None
    }
    let schemas = config->selectSchemas(schemaName)
    GeneratedArtifacts.sync(
      config,
      ~selectedSchemas=schemas,
      ~configDir=Process.process->Process.cwd,
    )
    let showSchemaName = !config.legacy || schemas->Array.length > 1
    let timeStarts: Dict.t<float> = dict{}
    let _watchers = schemas->Array.map(schema =>
      Utils.setupWatcher(
        ~onResult=(schema, res) => {
          switch timeStarts->Dict.get(schema.name) {
          | Some(timeStart) =>
            switch res {
            | Utils.GeneratorProcessFailure => ()
            | Utils.GeneratorResult(Error({errors})) =>
              if showSchemaName {
                Console.error(`[${schema.name}] Schema generation failed.`)
              }
              ErrorPrinter.printErrors(errors)
            | Utils.GeneratorResult(Success(_)) =>
              printBuildTime(schema, performance->now -. timeStart, ~showSchemaName)
              printAuthorizationBaselineWarning(schema.authorization)
            | Utils.GeneratorResult(_) =>
              Console.error(`[${schema.name}] Unexpected generator response.`)
            }
          | None => ()
          }
        },
        ~onStartRebuild=schema => {
          timeStarts->Dict.set(schema.name, performance->now)
          if showSchemaName {
            Console.log(`[${schema.name}] Rebuilding.`)
          } else {
            Console.log("Rebuilding.")
          }
        },
        ~config=schema,
      )
    )
    Console.log("Watching for changes...")
  | list{"lsp", configFilePath} => Lsp.start(~configFilePath, ~mode=Lsp.Stdio)
  | list{"tools", "find-definition", ...rest} =>
    switch parseFindDefinitionArgs(rest) {
    | Some({target, jsonOutput, schemaName}) =>
      printFindDefinition(~target, ~jsonOutput, ~schemaName)
    | None =>
      Console.error("Invalid tools arguments.")
      Console.log(toolsHelpText)
      Process.process->Process.exitWithCode(1)
    }
  | list{"help"} => Console.log(helpText)
  | list{"tools"} => Console.log(toolsHelpText)
  | value =>
    Console.error("Invalid command: " ++ value->List.toArray->Array.join(" "))
    Console.log(helpText)
    Process.process->Process.exitWithCode(1)
  }
} catch {
| Exn.Error(error) =>
  Console.error(error)
  Process.process->Process.exitWithCode(1)
| _ =>
  Console.error("Error!")
  Process.process->Process.exitWithCode(1)
}
