(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml, [ocamlformat-mlx] for OCaml.mlx, and
    [refmt] for Reason. *)

open Import

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

val message : error -> string
val run : Document.t -> Fiber.Cancel.t option -> (TextEdit.t list, error) result Fiber.t
