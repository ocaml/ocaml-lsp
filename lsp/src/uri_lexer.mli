type t =
  { scheme : string
  ; authority : string
  ; path : string
  }

val of_string : string -> t

val of_path : string -> t
