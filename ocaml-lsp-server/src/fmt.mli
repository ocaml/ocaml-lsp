open Import

(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml and [refmt] for reason *)

type error =
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Lsp.Uri.t

val message : error -> string

val run : refmt_width:int -> Document.t -> (string, error) Result.t
