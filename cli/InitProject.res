type projectIssues =
  | MissingConfigFile
  | ConfigFileIssue
  | OutputFolderDoesNotExist({schemaName: string, path: string})
  | ProjectRootDoesNotExist({schemaName: string, path: string})
  | IncludePathDoesNotExist({schemaName: string, path: string})
  | DuplicateOutputFolder({firstSchema: string, secondSchema: string, path: string})
  | DuplicateModuleName({firstSchema: string, secondSchema: string, moduleName: string})
  | DuplicateSchemaStateName({firstSchema: string, secondSchema: string})
  | DefaultSchemaDoesNotExist({schemaName: string})
  | InvalidSchemaName({schemaName: string})
  | InvalidModuleName({schemaName: string, moduleName: string})
  | InvalidContextType({schemaName: string, contextType: string})
  | IncludePathOutsideProject({schemaName: string, path: string, projectRoot: string})

module Console = Stdlib.Console
module JsExn = Js.Exn

@module("node:fs") external realpathSync: string => string = "realpathSync"

type readFileError = FileDoesNotExist | FileCouldNotBeRead(option<string>)
let readFile = (relativePath, ~dir) => {
  let path = Path.resolve([dir, relativePath])

  if Fs.existsSync(path) {
    try {
      path->Fs.readFileSync->Buffer.toStringWithEncoding(StringEncoding.utf8)->Ok
    } catch {
    | Exn.Error(_) => Error(FileCouldNotBeRead(None))
    }
  } else {
    Error(FileDoesNotExist)
  }
}

let validSchemaName: string => bool = %raw(`value => /^[A-Za-z0-9_-]+$/.test(value)`)
let validModuleName: string => bool = %raw(`value => /^[A-Z][A-Za-z0-9_]*$/.test(value)`)
let validContextType: string => bool = %raw(`value => /^[A-Z][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)+$/.test(value)`)

let pathIsWithin = (path, root) => path === root || path->String.startsWith(root ++ Path.sep)

let filesystemIdentity = path => {
  let canonical = try {
    realpathSync(path)
  } catch {
  | _ => path
  }
  canonical->String.toLowerCase
}

let compilerRoot = projectRoot =>
  projectRoot->Utils.findCompilerRoot->Option.getOr(projectRoot)->filesystemIdentity

let validateConfig = (config: Utils.config, ~issues) => {
  if config->Utils.findSchema(config.defaultSchema)->Option.isNone {
    issues->Array.push(DefaultSchemaDoesNotExist({schemaName: config.defaultSchema}))
  }

  config.schemas->Array.forEach(schema => {
    if !validSchemaName(schema.name) {
      issues->Array.push(InvalidSchemaName({schemaName: schema.name}))
    }
    if !validModuleName(schema.moduleName) {
      issues->Array.push(
        InvalidModuleName({schemaName: schema.name, moduleName: schema.moduleName}),
      )
    }
    if !validContextType(schema.contextType) {
      issues->Array.push(
        InvalidContextType({schemaName: schema.name, contextType: schema.contextType}),
      )
    }
    if !Fs.existsSync(schema.outputFolder) {
      issues->Array.push(
        OutputFolderDoesNotExist({schemaName: schema.name, path: schema.outputFolder}),
      )
    }
    if !Fs.existsSync(schema.projectRoot) {
      issues->Array.push(
        ProjectRootDoesNotExist({schemaName: schema.name, path: schema.projectRoot}),
      )
    }
    schema.includePaths->Array.forEach(path => {
      if !Fs.existsSync(path) {
        issues->Array.push(IncludePathDoesNotExist({schemaName: schema.name, path}))
      }
      if !(path->pathIsWithin(schema.projectRoot)) {
        issues->Array.push(
          IncludePathOutsideProject({
            schemaName: schema.name,
            path,
            projectRoot: schema.projectRoot,
          }),
        )
      }
    })
  })

  config.schemas->Array.forEachWithIndex((schema, index) =>
    config.schemas->Array.forEachWithIndex((otherSchema, otherIndex) => {
      if (
        otherIndex > index &&
          schema.outputFolder->filesystemIdentity === otherSchema.outputFolder->filesystemIdentity
      ) {
        issues->Array.push(
          DuplicateOutputFolder({
            firstSchema: schema.name,
            secondSchema: otherSchema.name,
            path: schema.outputFolder,
          }),
        )
      }
      if (
        otherIndex > index &&
        schema.projectRoot->compilerRoot === otherSchema.projectRoot->compilerRoot &&
        schema.moduleName->String.toLowerCase === otherSchema.moduleName->String.toLowerCase
      ) {
        issues->Array.push(
          DuplicateModuleName({
            firstSchema: schema.name,
            secondSchema: otherSchema.name,
            moduleName: schema.moduleName,
          }),
        )
      }
      if (
        otherIndex > index &&
        schema.projectRoot->compilerRoot === otherSchema.projectRoot->compilerRoot &&
        schema.name->String.toLowerCase === otherSchema.name->String.toLowerCase
      ) {
        issues->Array.push(
          DuplicateSchemaStateName({
            firstSchema: schema.name,
            secondSchema: otherSchema.name,
          }),
        )
      }
    })
  )
}

let validateProject = dir => {
  let issues = []

  switch readFile("./resgraph.json", ~dir) {
  | Error(_) => issues->Array.push(MissingConfigFile)
  | Ok(configFileContents) =>
    let config = try configFileContents->JSON.parseOrThrow->Utils.parseConfig(~baseDir=dir) catch {
    | _ => None
    }

    switch config {
    | None => issues->Array.push(ConfigFileIssue)
    | Some(config) => config->validateConfig(~issues)
    }
  }

  issues
}

let printProjectIssues = issues => {
  Console.error("⛔ One or more issues was encountered with this ResGraph project.\n")
  issues->Array.forEach(issue =>
    switch issue {
    | MissingConfigFile =>
      Console.error(`- 🚫 The file "resgraph.json" does not exist in this project root. Please create it and add + configure the content below to your liking:
{
  "src": "./src",
  "outputFolder": "./src/__generated__"
}`)
    | ConfigFileIssue =>
      Console.error(`- 🚫 "resgraph.json" could not be parsed. Please check its schema configuration.`)
    | OutputFolderDoesNotExist({schemaName, path}) =>
      Console.error(
        `- 🚫 Schema "${schemaName}" outputFolder "${path}" does not exist or cannot be accessed.`,
      )
    | ProjectRootDoesNotExist({schemaName, path}) =>
      Console.error(
        `- 🚫 Schema "${schemaName}" projectRoot "${path}" does not exist or cannot be accessed.`,
      )
    | IncludePathDoesNotExist({schemaName, path}) =>
      Console.error(
        `- 🚫 Schema "${schemaName}" include path "${path}" does not exist or cannot be accessed.`,
      )
    | DuplicateOutputFolder({firstSchema, secondSchema, path}) =>
      Console.error(
        `- 🚫 Schemas "${firstSchema}" and "${secondSchema}" use the same outputFolder "${path}".`,
      )
    | DuplicateModuleName({firstSchema, secondSchema, moduleName}) =>
      Console.error(
        `- 🚫 Schemas "${firstSchema}" and "${secondSchema}" use moduleName "${moduleName}" in the same ReScript package.`,
      )
    | DuplicateSchemaStateName({firstSchema, secondSchema}) =>
      Console.error(
        `- 🚫 Schemas "${firstSchema}" and "${secondSchema}" have names that collide on case-insensitive filesystems in the same ReScript package.`,
      )
    | DefaultSchemaDoesNotExist({schemaName}) =>
      Console.error(`- 🚫 defaultSchema "${schemaName}" does not name a configured schema.`)
    | InvalidSchemaName({schemaName}) =>
      Console.error(
        `- 🚫 Schema name "${schemaName}" may contain only letters, numbers, underscores, and hyphens.`,
      )
    | InvalidModuleName({schemaName, moduleName}) =>
      Console.error(
        `- 🚫 Schema "${schemaName}" moduleName "${moduleName}" is not a valid ReScript module name.`,
      )
    | InvalidContextType({schemaName, contextType}) =>
      Console.error(
        `- 🚫 Schema "${schemaName}" contextType "${contextType}" is not a valid qualified ReScript type.`,
      )
    | IncludePathOutsideProject({schemaName, path, projectRoot}) =>
      Console.error(
        `- 🚫 Schema "${schemaName}" include path "${path}" is outside projectRoot "${projectRoot}".`,
      )
    }
  )
}
