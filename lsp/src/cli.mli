module Channel : sig
  type t =
    | Stdio
    | Pipe of string
    | Socket of int
end

module Arg : sig
  type t

  val create : unit -> t

  val spec : t -> (string * Arg.spec * string) list

  val read : t -> (Channel.t, string) result
end
