open! Import

(** A URI with normalized encoded components. All constructors use the same
    normalization rules, so equal URIs have the same representation and spelling. *)
type t

include Json.Jsonable.S with type t := t

(** Structural comparison of normalized URI components. *)
val compare : t -> t -> int

val equal : t -> t -> bool
val hash : t -> int

(** Return the URI's filesystem path. Query and fragment components are ignored. *)
val to_path : t -> string

(** Construct a normalized file URI from a filesystem path, escaping literal
    characters rather than treating them as URI syntax. *)
val of_path : string -> t

(** Return the normalized URI spelling. *)
val to_string : t -> string

(** Parse and normalize a URI. Scheme case, unreserved escapes, and escape digit
    case are normalized. File URIs additionally normalize authority spelling and
    local drive-letter case, and encode their decoded filesystem paths just as
    {!of_path} does. Reserved escapes in other components and absent versus empty
    query/fragment delimiters remain significant. *)
val of_string : string -> t

(** Return the percent-decoded query, without interpreting query parameters. *)
val query : t -> string option

(** Return the percent-decoded fragment. *)
val fragment : t -> string option

module Private : sig
  val win32 : bool ref
end
