open Import
open Types

type t =
  | Begin of WorkDoneProgressBegin.t
  | Report of WorkDoneProgressReport.t
  | End of WorkDoneProgressEnd.t

val yojson_of_t : t -> Json.t
val t_of_yojson : Json.t -> t

(** Attempt a lossless work-done conversion. Return [None] if decoding fails or
    re-encoding would lose fields or explicit nulls. Object key order is ignored. *)
val t_of_yojson_opt : Json.t -> t option

val method_ : string
