open Import

type t =
  { wheel : Lev_fiber.Timer.Wheel.t
  ; data : Config_data.t
  }

val default : unit -> t Fiber.t
val wheel : t -> Lev_fiber.Timer.Wheel.t
val update : t -> DidChangeConfigurationParams.t -> t Fiber.t
val display_merlin_diagnostics : t -> bool
val shorten_merlin_diagnostics : t -> bool
