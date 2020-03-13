open Import

(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml and [refmt] for reason *)

type error =
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of { name : string }

val message : error -> string

val run : fname:string -> contents:string -> (string, error) Result.t
