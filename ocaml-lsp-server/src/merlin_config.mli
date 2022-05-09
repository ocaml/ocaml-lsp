open Import

module Ref : sig
  type t

  val config : t -> Mconfig.t Fiber.t
end

type t

val create : unit -> t

val stop : t -> unit Fiber.t

val run : t -> unit Fiber.t

val get : t -> Uri.t -> Ref.t
