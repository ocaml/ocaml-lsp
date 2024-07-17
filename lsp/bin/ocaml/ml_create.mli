(* Generate create functions with optional/labeled arguments *)
val intf_of_type : Ml.Type.decl Named.t -> Ml.Module.sig_ Named.t list
val impl_of_type : Ml.Type.decl Named.t -> Ml.Module.impl Named.t list
