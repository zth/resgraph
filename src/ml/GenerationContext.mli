type t

val create : unit -> t
val loadCmt : t -> moduleName:string -> path:string -> CmtDirect.t option
val seedSummary :
  t ->
  package:SharedTypes.package ->
  moduleName:string ->
  SharedTypes.File.t ->
  unit
val loadSummary :
  t ->
  package:SharedTypes.package ->
  moduleName:string ->
  SharedTypes.File.t option
