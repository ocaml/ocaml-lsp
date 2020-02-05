open Import

type opts = (string * string option) list

val exec: string -> opts -> (string, string) Result.t
