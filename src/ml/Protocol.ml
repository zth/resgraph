type position = {line: int; character: int}
type range = {start: position; end_: position}

type diagnostic = {range: range; message: string; severity: int}

let stringifyRange {start; end_} =
  Printf.sprintf
    {|{"start":{"line":%d,"character":%d},"end":{"line":%d,"character":%d}}|}
    start.line start.character end_.line end_.character

let stringifyDiagnostic {range; message; severity} =
  let esc = String.escaped in
  Printf.sprintf {|{"range":%s,"message":"%s","severity":%d}|}
    (stringifyRange range) (esc message) severity
