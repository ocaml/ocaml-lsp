type t =
  { scheme : string option
  ; authority : string
  ; path : string
  }

val of_string : string -> t
