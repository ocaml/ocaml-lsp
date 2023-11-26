(** Tries the parse the incoming string for a prefix. The string should be the
    source code ending at the prefix position. pos and len set the range for the
    regex to operate on *)
val parse : pos:int -> len:int -> string -> string option
