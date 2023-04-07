// Holds LSP protocol stuff + helpers.
type loc = {
  line: int,
  character: int,
}

type range = {
  start: loc,
  @as("end") end_: loc,
}
