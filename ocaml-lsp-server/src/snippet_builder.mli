open Import

type t =
  { snippet : Snippet.t
  ; placeholders : int
  }

(** Convert expression holes in OCaml source to snippet placeholders while
    leaving wildcard patterns unchanged. [placeholders] counts the generated
    placeholders. If parsing fails, the source is preserved without placeholders
    and [placeholders] is zero. A final tab stop is always appended. *)
val source : source:string -> t

(** A whole-call snippet with one hole per top-level function argument, derived
    by parsing a rendered OCaml core type. Requires at least two labelled or
    optional arguments; positional arguments do not count toward this threshold.
    Returns [None] for other types, unsafe names, or unparseable types. *)
val application : name:string -> typ:string -> Snippet.t option
