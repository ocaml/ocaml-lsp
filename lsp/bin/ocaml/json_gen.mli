val json_t : Ml.Type.t

val add_json_conv_for_t :
  Ml.Module.sig_ Ml.Module.t -> Ml.Module.sig_ Ml.Module.t

module Enum : sig
  val conv :
       allow_other:bool
    -> poly:bool
    -> (string * Ts_types.Literal.t) list Named.t
    -> Ml.Expr.toplevel Named.t list
end

module Poly_variant : sig
  val of_json : Ml.Type.constr list Named.t -> Ml.Expr.toplevel Named.t

  val to_json : Ml.Type.constr list Named.t -> Ml.Expr.toplevel Named.t
end

val make_literal_wrapper_conv :
     field_name:string
  -> literal_value:string
  -> type_name:string
  -> Ml.Module.impl Named.t list
