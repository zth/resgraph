type resolveInfo = unit
module Authorization = {
  type coverage = Selection
  type coverageOptions = {covers: coverage}

  type outcome<'value, 'reason> = Allowed('value) | Forbidden('reason)
}
