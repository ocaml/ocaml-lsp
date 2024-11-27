type t =
  { scheme : string
  ; authority : string
  ; path : string
  ; query : string option
  ; fragment : string option
  }

val of_string : string -> t
val of_path : string -> t
