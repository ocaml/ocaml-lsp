(** URI syntax with encoded components and explicit delimiter presence. *)
type t =
  { scheme : string option
  ; authority : string option
  ; path : string
  ; query : string option
  ; fragment : string option
  }

val of_string : string -> t

(** Split a filesystem path into a raw UNC authority and an absolute path.
    Neither part is percent-decoded or encoded. *)
val of_path : string -> string * string

val is_unreserved : char -> bool

(** Decode once. [only_unreserved] normalizes URI spelling instead: it keeps
    reserved escapes encoded, uppercases escape digits, and quotes malformed '%'
    bytes so decoding neighbouring escapes cannot introduce a new escape. *)
val decode : ?only_unreserved:bool -> string -> string
