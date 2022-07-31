module Module : sig
  type t = (Ml.Module.sig_ Ml.Module.t, Ml.Module.impl Ml.Module.t) Ml.Kind.pair

  val pp : t -> unit Pp.t Ml.Kind.Map.t
end

val of_typescript : Ts_types.Unresolved.t list -> Module.t list
