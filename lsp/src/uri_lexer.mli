type t =
  { scheme : string option
  ; authority : string option
  ; path : string
  }

val of_string : string -> t
