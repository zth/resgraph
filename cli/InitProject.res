type projectIssues =
  | /** Misses `resgraph.json`*/ MissingConfigFile
  | /** Config file has issues*/ ConfigFileIssue
  | OutputFolderDoesNotExist({path: string})
  | SrcFolderDoesNotExist({path: string})
  | /** Misses `ResGraphContext.res` */ MissingContextFile

type readFileError = FileDoesNotExist | FileCouldNotBeRead(option<string>)
let readFile = (relativePath, ~dir) => {
  let path = Path.resolve([dir, relativePath])

  if Fs.existsSync(path) {
    try {
      path->Fs.readFileSync->Buffer.toStringWithEncoding(StringEncoding.utf8)->Ok
    } catch {
    | Exn.Error(err) => Error(FileCouldNotBeRead(err->Exn.message))
    }
  } else {
    Error(FileDoesNotExist)
  }
}

let validateConfig = (config: Utils.config, ~issues) => {
  if !Fs.existsSync(config.outputFolder) {
    issues->Array.push(OutputFolderDoesNotExist({path: config.outputFolder}))
  }
  if !Fs.existsSync(config.src) {
    issues->Array.push(SrcFolderDoesNotExist({path: config.outputFolder}))
  }
}

let validateProject = dir => {
  let readFile = readFile(~dir, ...)
  let issues = []

  switch readFile("./resgraph.json") {
  | Error(_) => issues->Array.push(MissingConfigFile)
  | Ok(configFileContents) =>
    let config = try configFileContents->JSON.parseExn->Utils.parseConfig catch {
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
    | MissingContextFile => /* TODO: Ask priv bin for whether assets exist */ ()
    | OutputFolderDoesNotExist({path}) =>
      Console.error(
        `- 🚫 "outputFolder" in "resgraph.json" is configured to be "${path}", but that folder either does not exist, or is not possible to access.`,
      )
    | ConfigFileIssue =>
      /* TODO: Link to docs */
      Console.error(`- 🚫 "resgraph.json" exists but contains issues. Please double check it's configured correctly.`)
    | SrcFolderDoesNotExist({path}) =>
      Console.error(
        `- 🚫 "src" in "resgraph.json" is configured to be "${path}", but that folder either does not exist, or is not possible to access.`,
      )
    }
  )
}
