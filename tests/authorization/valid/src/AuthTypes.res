type authResult<'value, 'reason> = ResGraph.Authorization.outcome<'value, 'reason>
type asyncAuthResult<'value, 'reason> = promise<ResGraph.Authorization.outcome<'value, 'reason>>
