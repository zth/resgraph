module Console = Stdlib.Console

type removeOptions = {recursive: bool, force: bool}

@module("node:fs")
external mkdtempSync: string => string = "mkdtempSync"

@module("node:fs")
external rmSync: (string, removeOptions) => unit = "rmSync"

let insertAnnotations: (string, array<int>, string) => string = %raw(`
  (source, targetLines, annotation) => {
    const newline = source.includes("\r\n") ? "\r\n" : "\n"
    const lines = source.split(/\r?\n/)
    const uniqueLines = [...new Set(targetLines)].sort((a, b) => b - a)
    for (const lineNumber of uniqueLines) {
      const target = lines[lineNumber]
      if (target === undefined) {
        throw new Error("Authorization bootstrap received an invalid source location")
      }
      const indentation = target.match(/^[\t ]*/)[0]
      lines.splice(lineNumber, 0, indentation + annotation)
    }
    return lines.join(newline)
  }
`)

let nonWhitespaceLength: string => int = %raw(`
  reason => reason.replace(/\s/g, "").length
`)

let defaultReason = "Pending required authorization migration"

let isMissingDisposition = message =>
  message->String.startsWith("Field `") &&
    message->String.includes("has no authorization disposition")

let isUnsupportedSubscription = message =>
  message === "Required authorization coverage does not support subscriptions yet."

let isOutcomeOnlyMutation = message =>
  message->String.startsWith("Mutation field `") &&
    message->String.includes("requires at least one pre-resolver")

let isBootstrapCandidate = (error: Utils.generateError) =>
  isMissingDisposition(error.message) ||
  isUnsupportedSubscription(error.message) ||
  isOutcomeOnlyMutation(error.message)

let writeAnnotations = (~errors: array<Utils.generateError>, ~reason) => {
  let encodedReason =
    reason
    ->JSON.stringifyAny
    ->Option.getOrThrow(~message="Could not encode the authorization bootstrap reason")
  let annotation = `@gql.authorizationUnchecked({reason: ${encodedReason}})`
  let files: Dict.t<array<int>> = Dict.make()

  errors->Array.forEach(error => {
    let lines = files->Dict.get(error.file)->Option.getOr([])
    files->Dict.set(error.file, lines->Array.concat([error.range.start.line]))
  })

  let updates =
    files
    ->Dict.toArray
    ->Array.map(((file, targetLines)) => {
      let source =
        file
        ->Fs.readFileSync
        ->Buffer.toStringWithEncoding(StringEncoding.utf8)
      (file, insertAnnotations(source, targetLines, annotation))
    })

  updates->Array.forEach(((file, updated)) =>
    Fs.writeFileSyncWith(file, Buffer.fromString(updated), {encoding: "utf-8"})
  )

  files->Dict.size
}

let run = (~config: Utils.config, ~reason=defaultReason) => {
  if nonWhitespaceLength(reason) < 3 {
    Console.error("The bootstrap reason must contain at least 3 non-whitespace characters.")
    Process.process->Process.exitWithCode(1)
    panic("Unreachable after process exit")
  }

  switch config.authorization {
  | Some(_) => ()
  | None =>
    Console.error(
      "Enable authorization mode `required` in resgraph.json before running the bootstrap codemod.",
    )
    Process.process->Process.exitWithCode(1)
  }

  let tempOutput = Path.join([Os.tmpdir(), "resgraph-authorization-bootstrap-"])->mkdtempSync
  let result = try {
    Utils.callPrivateCli(BootstrapAuthorization({src: config.src, outputFolder: tempOutput}))
  } catch {
  | exn =>
    rmSync(tempOutput, {recursive: true, force: true})
    throw(exn)
  }
  rmSync(tempOutput, {recursive: true, force: true})

  switch result {
  | Success(_) => Console.log("All executable fields already have authorization coverage.")
  | Error({errors}) =>
    let blockers = errors->Array.filter(error => !isBootstrapCandidate(error))
    if blockers->Array.length > 0 {
      Console.error(
        "Authorization bootstrap stopped because schema generation has unrelated errors.",
      )
      ErrorPrinter.printErrors(blockers)
      Process.process->Process.exitWithCode(1)
    }

    let candidates = errors->Array.filter(isBootstrapCandidate)
    let fileCount = writeAnnotations(~errors=candidates, ~reason)
    Console.log(
      `Added @gql.authorizationUnchecked to ${candidates
        ->Array.length
        ->Int.toString} fields across ${fileCount->Int.toString} files.`,
    )
    Console.log("Recompile the project, then run `resgraph build` to verify required coverage.")
  | Completion(_) | Hover(_) | Definition(_) | FindDefinition(_) | NotInitialized =>
    Console.error("Unexpected response from the ResGraph authorization bootstrap command.")
    Process.process->Process.exitWithCode(1)
  }
}
