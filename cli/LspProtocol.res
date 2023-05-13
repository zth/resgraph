// Holds LSP protocol stuff + helpers.
type loc = {
  line: int,
  character: int,
}

type range = {
  start: loc,
  @as("end") end_: loc,
}

@live
type location = {
  uri: string,
  range: range,
}

@live
type diagnostic = {
  range: range,
  message: string,
  source: string,
}

let makeDiagnostic = (~range, ~message) => {
  range,
  message,
  source: "RescriptRelayRouter",
}

@live
type markupContent = {
  kind: [#plaintext | #markdown],
  value: string,
}

@live
type hover = {
  contents: markupContent,
  range: option<range>,
}

let makeHover = (~message, ~loc) => {
  contents: {
    kind: #markdown,
    value: message,
  },
  range: Some(loc),
}

@live
type completionItemKind =
  | Text
  | Method
  | Function
  | Constructor
  | Field
  | Variable
  | Class
  | Interface
  | Module
  | Property
  | Unit
  | Value
  | Enum
  | Keyword
  | Snippet
  | Color
  | File
  | Reference
  | Folder
  | EnumMember
  | Constant
  | Struct
  | Event
  | Operator
  | TypeParameter

@live
let completionItemToInt = item =>
  switch item {
  | Text => 1
  | Method => 2
  | Function => 3
  | Constructor => 4
  | Field => 5
  | Variable => 6
  | Class => 7
  | Interface => 8
  | Module => 9
  | Property => 10
  | Unit => 11
  | Value => 12
  | Enum => 13
  | Keyword => 14
  | Snippet => 15
  | Color => 16
  | File => 17
  | Reference => 18
  | Folder => 19
  | EnumMember => 20
  | Constant => 21
  | Struct => 22
  | Event => 23
  | Operator => 24
  | TypeParameter => 25
  }

@live
type completionItem = {label: string, kind: completionItemKind}

let makeCompletionItem = (~label, ~kind) => {
  label,
  kind,
}

@live
type command = {
  title: string,
  command: string,
  arguments: option<array<string>>,
}

@live
type codeLens = {
  range: range,
  command: command,
}

type codeActionKind =
  | Empty
  | QuickFix
  | Refactor
  | RefactorExtract
  | RefactorInline
  | RefactorRewrite
  | Source
  | SourceOrganizeImports
  | SourceFixAll

let codeActionKindToString = kind =>
  switch kind {
  | Empty => ""
  | QuickFix => "quickfix"
  | Refactor => "refactor"
  | RefactorExtract => "refactor.extract"
  | RefactorInline => "refactor.inline"
  | RefactorRewrite => "refactor.rewrite"
  | Source => "source"
  | SourceOrganizeImports => "source.organizeImports"
  | SourceFixAll => "source.fixAll"
  }

type textEdit = {
  range: range,
  newText: string,
}

module DocumentChange: {
  type t
  module CreateFile: {
    let make: (~uri: string, ~overwrite: bool=?, ~ignoreIfExists: bool=?, unit) => t
  }
  module TextDocumentEdit: {
    let make: (~textDocumentUri: string, ~edits: array<textEdit>) => t
  }
} = {
  type t
  external cast: 'any => t = "%identity"

  module CreateFile = {
    let make = (~uri, ~overwrite=?, ~ignoreIfExists=?, ()) =>
      {
        "kind": "create",
        "uri": uri,
        "options": {
          "overwrite": overwrite,
          "ignoreIfExists": ignoreIfExists,
        },
      }->cast
  }

  module TextDocumentEdit = {
    let make = (~textDocumentUri, ~edits) =>
      {
        "textDocument": {
          "uri": textDocumentUri,
        },
        "edits": edits,
      }->cast
  }
}

type workspaceEdit = {documentChanges: option<array<DocumentChange.t>>}

type codeAction = {
  title: string,
  kind: option<string>,
  isPreferred: option<bool>,
  edit: option<workspaceEdit>,
}

module Command = {
  @live
  let makeOpenFileCommand = (~title, ~fileUri) => {
    title,
    command: `vscode.open`,
    arguments: Some([fileUri]),
  }

  let makeOpenFileAtPosCommand = (~title, ~fileUri, ~pos) => {
    title,
    command: `vscode-rescript-relay.open-pos-in-doc`,
    arguments: Some([fileUri, pos.line->Int.toString, pos.character->Int.toString]),
  }

  type routeRendererReference = {
    sourceFilePath: string,
    routeName: string,
    loc: loc,
    routeRendererFilePath: string,
  }

  let makeOpenRouteDefinitionsCommand = (~title, ~routes) => {
    title,
    command: `vscode-rescript-relay.open-route-definitions`,
    arguments: Some(
      routes->Array.map(r =>
        `${r.sourceFilePath};${r.routeName};${r.loc.line->Int.toString};${r.loc.character->Int.toString};${r.routeRendererFilePath}`
      ),
    ),
  }

  let makeTextOnlyCommand = title => {
    title,
    command: "",
    arguments: None,
  }
}

let makeCodeLensItem = (~range, ~command) => {
  range,
  command,
}

@live
type documentLink = {
  range: range,
  target: string,
  tooltip: option<string>,
}

let makeDocumentLink = (~range, ~fileUri, ~tooltip=?, ()) => {
  range,
  target: fileUri,
  tooltip,
}

@live type definition = location
