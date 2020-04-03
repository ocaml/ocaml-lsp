(** Trivial parser to extract ```typescript sections out of markdown docs. *)
open! Import

val read_typescript : Lexing.lexbuf -> string list
(** Extracts all typescript sections *)
