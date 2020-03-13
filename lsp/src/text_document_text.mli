open Gprotocol

val normalize_line_endings : string -> string

val apply_change : string -> Range.t -> string -> string
