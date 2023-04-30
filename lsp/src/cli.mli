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

  val clientProcessId : t -> int option
end

(** generate command line arguments that can be used to spawn an lsp client  *)
val args : ?channel:Channel.t -> ?clientProcessId:int -> unit -> string list
