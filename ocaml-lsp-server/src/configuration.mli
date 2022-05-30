open Import

type t

val default : unit -> t Fiber.t

val wheel : t -> Lev_fiber.Timer.Wheel.t

val update : t -> DidChangeConfigurationParams.t -> t Fiber.t
