type resolveInfo = unit
module Authorization = {
  type outcome<'value, 'reason> = Allowed('value) | Forbidden('reason)
}
