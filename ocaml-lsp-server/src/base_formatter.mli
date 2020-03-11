open Stdune

val read_to_end : in_channel -> string

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

val run_command : string -> ?stdin_value:string -> string list -> command_result

val _PATH : Path.t list lazy_t

type error =
  | Missing_binary
  | Message of string
