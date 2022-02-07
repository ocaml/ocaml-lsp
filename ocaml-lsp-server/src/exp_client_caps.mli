open Import

(** Module to store experimental client capabilities *)

type t

val of_opt_json : Json.t option -> t

val supportsJumpToNextHole : t -> bool
