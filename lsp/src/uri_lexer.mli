(** URI syntax with encoded components and explicit delimiter presence. *)
type t =
  { scheme : string option
  ; authority : string option
  ; path : string
  ; query : string option
  ; fragment : string option
  }

val of_string : string -> t

(** Split a leading UNC authority from a filesystem path using slash separators.
    Neither part is decoded, encoded, or otherwise normalized. *)
val split_unc_path : string -> string * string
