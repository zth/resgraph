@val
external argv: array<option<string>> = "process.argv"

let args = argv->Array.sliceToEnd(~start=2)->Array.keepSome
let argsList = args->List.fromArray

try {
  switch argsList {
  | list{"build", src, outputFolder} =>
    let res = Utils.callPrivateCli(GenerateSchema({src, outputFolder}))
    switch res {
    | Completion(_) | NotInitialized => ()
    | Success(_) => Console.log("Build succeeded.")
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
    }
  | list{"watch", src, outputFolder} =>
    let _ = Utils.setupWatcher(~onResult=_ => (), ~src, ~outputFolder)
  | list{"lsp", configFilePath} => Lsp.start(~configFilePath, ~mode=Lsp.Stdio)
  | v => Console.error("Invalid command: " ++ v->List.toArray->Array.joinWith(" "))
  }
} catch {
| Exn.Error(e) => Console.error("Error: " ++ e->Exn.message->Option.getWithDefault("-"))
| _ => Console.error("Error!")
}
