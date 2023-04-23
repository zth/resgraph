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
      path->Fs.readFileSync->Node.Buffer.toStringWithEncoding(#utf8)->Ok
    } catch {
    | Exn.Error(err) => Error(FileCouldNotBeRead(err->Exn.message))
    }
  } else {
    Error(FileDoesNotExist)
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
    | Some(config) =>
      if !Fs.existsSync(config.outputFolder) {
        issues->Array.push(OutputFolderDoesNotExist({path: config.outputFolder}))
      }
      if !Fs.existsSync(config.src) {
        issues->Array.push(SrcFolderDoesNotExist({path: config.outputFolder}))
      }
    }
  }

  issues
}
