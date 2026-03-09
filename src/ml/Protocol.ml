type position = {line: int; character: int}
type range = {start: position; end_: position}
type markupContent = {kind: string; value: string}
type insertTextFormat = Snippet

type completionItem = {
  label: string;
  kind: int;
  tags: int list;
  detail: string;
  sortText: string option;
  filterText: string option;
  insertTextFormat: insertTextFormat option;
  insertText: string option;
  documentation: markupContent option;
}

type location = {uri: string; range: range}
type diagnostic = {range: range; message: string; severity: int}

let null = "null"
let array items = "[" ^ String.concat ", " items ^ "]"
let wrapInQuotes value = "\"" ^ Json.escape value ^ "\""

let optWrapInQuotes value =
  match value with
  | None -> None
  | Some value -> Some (wrapInQuotes value)

let stringifyPosition {line; character} =
  Printf.sprintf {|{"line":%d,"character":%d}|} line character

let stringifyRange {start; end_} =
  Printf.sprintf {|{"start":%s,"end":%s}|} (stringifyPosition start)
    (stringifyPosition end_)

let stringifyMarkupContent {kind; value} =
  Printf.sprintf {|{"kind":"%s","value":"%s"}|} kind (Json.escape value)

let stringifyObject properties =
  "{"
  ^ (properties
    |> List.filter_map (fun (key, value) ->
        match value with
        | None -> None
        | Some value -> Some (Printf.sprintf {|"%s":%s|} key value))
    |> String.concat ",")
  ^ "}"

let insertTextFormatToInt = function
  | Snippet -> 2

let stringifyCompletionItem completionItem =
  stringifyObject
    [
      ("label", Some (wrapInQuotes completionItem.label));
      ("kind", Some (string_of_int completionItem.kind));
      ("tags", Some (completionItem.tags |> List.map string_of_int |> array));
      ("detail", Some (wrapInQuotes completionItem.detail));
      ( "documentation",
        Some
          (match completionItem.documentation with
          | None -> null
          | Some documentation -> stringifyMarkupContent documentation) );
      ("sortText", optWrapInQuotes completionItem.sortText);
      ("filterText", optWrapInQuotes completionItem.filterText);
      ("insertText", optWrapInQuotes completionItem.insertText);
      ( "insertTextFormat",
        match completionItem.insertTextFormat with
        | None -> None
        | Some insertTextFormat ->
          Some (string_of_int (insertTextFormatToInt insertTextFormat)) );
    ]

let stringifyHover value =
  Printf.sprintf {|{"contents":%s}|}
    (stringifyMarkupContent {kind = "markdown"; value})

let stringifyLocation {uri; range} =
  Printf.sprintf {|{"uri":"%s","range":%s}|} (Json.escape uri)
    (stringifyRange range)

let stringifyDiagnostic {range; message; severity} =
  Printf.sprintf {|{"range":%s,"message":"%s","severity":%d}|}
    (stringifyRange range) (Json.escape message) severity
