open Import

type t =
  { snippet : Snippet.t
  ; placeholders : int
  }

type hole_kind =
  [ `Expression
  | `After_arrow
  ]

(** Convert OCaml holes to snippet placeholders. [`Expression] converts
    expression holes while leaving wildcard patterns unchanged. [`After_arrow]
    converts only expression holes immediately following [->], including in
    generated case fragments. Type wildcards are left unchanged. [placeholders]
    counts the generated placeholders. If parsing or lexing fails, the source is
    preserved without placeholders and [placeholders] is zero. A final tab stop
    is always appended. *)
val source : holes:hole_kind -> source:string -> t

(** A whole-call snippet with one hole per top-level function argument, derived
    by parsing a rendered OCaml core type. Requires at least two labelled or
    optional arguments; positional arguments do not count toward this threshold.
    Returns [None] for other types, unsafe names, or unparseable types. *)
val application : name:string -> typ:string -> Snippet.t option
