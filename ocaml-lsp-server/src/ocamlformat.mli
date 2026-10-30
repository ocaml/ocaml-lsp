(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml, [ocamlformat-mlx] for OCaml.mlx and
    [refmt] for Reason, with [ocp-indent] and [topiary] as alternative OCaml
    formatters. For OCaml files the closest [.ocamlformat], [.ocp-indent], or
    [.topiary/languages.toml] between the document and its workspace root
    selects the formatter. When multiple configs are in the same directory
    [ocamlformat] wins, followed by [ocp-indent], then [topiary]
    ([ocamlformat] > [ocp-indent] > [topiary]). If none is configured,
    [ocamlformat] is preferred when available, then [ocp-indent], then
    [topiary] as fallback. *)

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
