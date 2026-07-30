type t = {buffer: Buffer.t; mutable indentation: int; mutable lineStart: bool}

let create capacity =
  {buffer = Buffer.create capacity; indentation = 0; lineStart = true}

let addIndent writer =
  if writer.lineStart then (
    Buffer.add_string writer.buffer (String.make writer.indentation ' ');
    writer.lineStart <- false)

let addChar writer char =
  if char = '\n' then (
    Buffer.add_char writer.buffer char;
    writer.lineStart <- true)
  else (
    addIndent writer;
    Buffer.add_char writer.buffer char)

let add writer text = String.iter (addChar writer) text

let line writer text =
  add writer text;
  addChar writer '\n'

let newline writer = addChar writer '\n'

let blankLine writer =
  if writer.lineStart then newline writer
  else (
    newline writer;
    newline writer)

let indented writer f =
  writer.indentation <- writer.indentation + 2;
  Fun.protect
    ~finally:(fun () -> writer.indentation <- writer.indentation - 2)
    f

let contents writer = Buffer.contents writer.buffer
