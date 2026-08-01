type resolveInfo = unit
module Authorization = {
  type scope = Fields
  type scopeOptions = {scope: scope}

  type outcome<'value, 'reason> = Allowed('value) | Forbidden('reason)
}
