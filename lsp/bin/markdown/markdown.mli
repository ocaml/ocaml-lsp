open! Import
(** Trivial parser to extract ```typescript sections out of markdown docs. *)

val read_typescript : Lexing.lexbuf -> string list
(** Extracts all typescript sections *)
