(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml, [ocamlformat-mlx] for OCaml.mlx, and
    [refmt] for Reason. For OCaml files, the closest [.ocamlformat] or
    [.ocp-indent] between the document and its workspace root selects the
    formatter. [ocamlformat] wins when both files are in the same directory. If
    neither is configured, [ocp-indent] is the fallback when [ocamlformat] is
    missing. *)

open Import

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

val message : error -> string

val run
  :  workspace_root:Uri.t option
  -> Document.Merlin.t
  -> Fiber.Cancel.t option
  -> (TextEdit.t list, error) result Fiber.t

val run_on_range
  :  workspace_root:Uri.t option
  -> Document.t
  -> Range.t
  -> Fiber.Cancel.t option
  -> (TextEdit.t list, error) result Fiber.t
