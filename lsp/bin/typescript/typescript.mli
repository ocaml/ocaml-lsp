open Import

val of_metamodel : Metamodel.t -> Ts_types.Unresolved.t list

val resolve_all
  :  Ts_types.Unresolved.t list
  -> Ts_types.Resolved.t list * Ts_types.Ident.t String.Map.t
