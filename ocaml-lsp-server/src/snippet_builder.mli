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
