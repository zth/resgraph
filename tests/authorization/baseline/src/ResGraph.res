type resolveInfo = unit

module Authorization = {
  type outcome<'value, 'reason> =
    | Allowed('value)
    | Forbidden('reason)

  type error = {message: string, code: string}
  let makeError = (~message, ~code): error => {message, code}
  let raiseError = (_error: error): 'value => failwith("raised")
}
