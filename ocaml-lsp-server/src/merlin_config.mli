(** Fetch merlin configuration from a project build system (usually dune) *)

open Import

type t

val prefer_dot_merlin : bool ref
val config : t -> Mconfig.t Fiber.t
val destroy : t -> unit Fiber.t

module DB : sig
  type config := t
  type t

  val create : unit -> t
  val stop : t -> unit Fiber.t
  val run : t -> unit Fiber.t
  val get : t -> Uri.t -> config
end
