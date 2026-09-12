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

type application_kind =
  [ `Function
  | `Constructor
  ]

(** An application snippet derived from a rendered type. Functions require at
    least two labelled or optional arguments and get one hole per argument.
    Constructor applications are parenthesized and get one hole per argument,
    including one for a tuple-valued or inline-record argument. Nullary
    constructors are excluded. Names must be identifier paths, not operators.
    Returns [None] for unsuitable types, unsafe names, or unparseable types. *)
val application : kind:application_kind -> name:string -> typ:string -> Snippet.t option
