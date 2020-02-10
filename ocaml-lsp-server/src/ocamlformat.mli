open Import

module Options: sig
  type t = (string * string option) list
  val to_string_array: t -> string array
end

val exec: string -> Options.t -> (string, string) Result.t
