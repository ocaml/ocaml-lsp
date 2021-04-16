val infer_intf_for_impl : Document.t -> string Fiber.t

val infer_intf : force_open_impl:bool -> State.t -> Document.t -> string Fiber.t
