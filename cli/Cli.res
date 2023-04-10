@val
external argv: array<option<string>> = "process.argv"

let args = argv->Array.sliceToEnd(~start=2)->Array.keepSome
let argsList = args->List.fromArray

try {
  switch argsList {
  | list{"watch", path, schemaOutputPath, assetsOutputPath} =>
    let _ = Utils.setupWatcher(~onResult=_ => (), ~path, ~schemaOutputPath, ~assetsOutputPath)
  | list{"lsp", configFilePath} => Lsp.start(~configFilePath, ~mode=Lsp.Stdio)
  | v => Console.error("Invalid command: " ++ v->List.toArray->Array.joinWith(" "))
  }
} catch {
| Exn.Error(e) => Console.error("Error: " ++ e->Exn.message->Option.getWithDefault("-"))
| _ => Console.error("Error!")
}
