open Import

(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml and [refmt] for reason *)

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

val message : error -> string

val run :
     Scheduler.t
  -> Scheduler.thread
  -> Document.t
  -> (string, error) result Fiber.t
