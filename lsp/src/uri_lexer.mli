type t =
  { scheme : string
  ; authority : string
  ; path : string
  }

val of_string : string -> t
