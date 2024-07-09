open Import
open Types

type t =
  | Begin of WorkDoneProgressBegin.t
  | Report of WorkDoneProgressReport.t
  | End of WorkDoneProgressEnd.t

val yojson_of_t : t -> Json.t
val t_of_yojson : Json.t -> t
val method_ : string
