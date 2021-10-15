type t

val create : unit -> t

val stop : t -> unit Fiber.t

val run : t -> unit Fiber.t

val get_external_config : t -> Mconfig.t -> string -> Mconfig.t Fiber.t
