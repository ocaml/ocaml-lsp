open Test.Import

type t

val start
  :  ?diagnostics:(string -> Dune_rpc.Private.Diagnostic.Event.t list list)
  -> ?progress:Dune_rpc.Private.Progress.t list
  -> string
  -> t

val root : t -> string
val runtime_dir : t -> string
val stop : t -> unit
