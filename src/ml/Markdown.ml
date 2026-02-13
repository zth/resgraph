let spacing = "\n```\n \n```\n"
let codeBlock code = Printf.sprintf "```rescript\n%s\n```" code

let graphqlCodeBlock code = Printf.sprintf "```graphql\n%s\n```" code
let divider = "\n---\n"

type link = {startPos: Protocol.position; file: string; label: string}

let linkToCommandArgs link =
  Printf.sprintf "[\"%s\",%i,%i]" link.file link.startPos.line
    link.startPos.character

let makeGotoCommand link =
  Printf.sprintf "[%s](command:vscode-resgraph.go_to_location?%s)" link.label
    (Uri.encodeURIComponent (linkToCommandArgs link))

let goToDefinitionText ~(loc : Location.t) ~fileUri =
  let line, character = Pos.ofLexing loc.loc_start in
  "\n"
  ^ makeGotoCommand
      {
        label = "Open ReScript code for this definition";
        file = Uri.toString fileUri;
        startPos = {line; character};
      }
