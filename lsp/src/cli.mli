(** Handling of standard lsp server command line arguments *)

module Channel : sig
  (** The channel the server shold use to listen for connections *)

  type t =
    | Stdio
    | Pipe of string (** A path to the unix domain socket or windows pipe *)
    | Socket of int (** A tcp connection on localhost with the port number *)
end

module Arg : sig
  (** Parsing of the standard commnad line arguments using [Stdlib.Arg] *)

  type t

  (** [create ()] create a new record for arguments *)
  val create : unit -> t

  (** [spec t] returns the spec that should be provided to [Stdlib.Arg] to
      populate [t] using the interpreted cli args *)
  val spec : t -> (string * Arg.spec * string) list

  (** [channel t] return the channel if correctly supplied. An error if the
      arguments were provided incorrectly. *)
  val channel : t -> (Channel.t, string) result

  (** Return the process id of the client used to run the lsp server if it was
      provided *)
  val clientProcessId : t -> int option
end

(** generate command line arguments that can be used to spawn an lsp client *)
val args : ?channel:Channel.t -> ?clientProcessId:int -> unit -> string list
