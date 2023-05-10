// Pretty print diagnostics.

module Chalk = {
  type t

  type colors = {
    red: string => string,
    blackBright: string => string,
    blueBright: string => string,
  }

  type modifiers = {bold: colors}

  @module("chalk")
  external colors: colors = "default"

  @module("chalk")
  external modifiers: modifiers = "default"
}

let prettyPrintDiagnostic = (~lines, ~diagnostic: Utils.generateError) => {
  open Chalk

  let fileLocText = colors.blackBright(
    `${Int.toString(diagnostic.range.start.line + 1)}:${Int.toString(
        diagnostic.range.start.character + 1,
      )}-${Int.toString(diagnostic.range.end_.line + 1)}:${Int.toString(
        diagnostic.range.end_.character + 1,
      )}`,
  )

  `${colors.red("Error in file:")} ${colors.blueBright(
      diagnostic.file,
    )}:${fileLocText}`->Console.log
  Console.log("\n")
  lines->Array.forEachWithIndex((line, index) => {
    if index > diagnostic.range.start.line - 5 && index < diagnostic.range.end_.line + 5 {
      let highlightOnThisLine =
        index >= diagnostic.range.start.line && index <= diagnostic.range.end_.line

      if highlightOnThisLine {
        let highlightStartOffset = if index == diagnostic.range.start.line {
          diagnostic.range.start.character
        } else {
          0
        }

        let highlightEndOffset = if index == diagnostic.range.end_.line {
          diagnostic.range.end_.character
        } else {
          line->String.length
        }

        let lineText =
          line->String.slice(~start=0, ~end=highlightStartOffset) ++
          modifiers.bold.red(
            line->String.slice(~start=highlightStartOffset, ~end=highlightEndOffset),
          ) ++
          line->String.sliceToEnd(~start=highlightEndOffset)

        Console.log(
          `  ${modifiers.bold.red(
              Int.toString(index + 1),
            )} ${colors.blackBright(`┆`)} ${lineText}`,
        )
      } else {
        Console.log(`  ${Int.toString(index + 1)} ${colors.blackBright(`┆`)} ${line}`)
      }
    }
  })

  Console.log("\n  " ++ diagnostic.message)
}

let printErrors = (errors: array<Utils.generateError>) => {
  let fileContentCache = Dict.make()

  errors->Array.forEach(error => {
    let fileContentLines = switch fileContentCache->Dict.get(error.file) {
    | Some(content) => content
    | None =>
      let contents =
        error.file->Fs.readFileSync->Node.Buffer.toStringWithEncoding(#utf8)->String.split(Os.eol)
      fileContentCache->Dict.set(error.file, contents)
      contents
    }

    prettyPrintDiagnostic(~lines=fileContentLines, ~diagnostic=error)
  })
}
