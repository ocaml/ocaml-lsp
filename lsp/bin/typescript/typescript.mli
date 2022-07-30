open Import

type test

val of_snippets : string list -> Ts_types.Unresolved.t list

val of_metamodel : Metamodel.t -> Ts_types.Unresolved.t list

val test_snippets : string list -> test list

val resolve_all :
     Ts_types.Unresolved.t list
  -> Ts_types.Resolved.t list * Ts_types.Ident.t String.Map.t

val pp_results : Format.formatter -> test list -> unit
