open! Import

type t

include Json.Jsonable.S with type t := t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

(** Return the URI's filesystem path. Query and fragment components are ignored. *)
val to_path : t -> string

(** Construct a canonical file URI from a filesystem path. *)
val of_path : string -> t

(** Return the URI with the same spelling supplied to {!of_string} or decoded from JSON. *)
val to_string : t -> string

(** Parse a URI while preserving its exact spelling. *)
val of_string : string -> t

val query : t -> string option
val fragment : t -> string option

module Private : sig
  val win32 : bool ref
end
