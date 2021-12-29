(** Low level module for sending/receiving jsonrpc packets across channels *)

exception Error of string

module Make (Io : sig
  type 'a t

  val return : 'a -> 'a t

  val raise : exn -> 'a t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end) (Chan : sig
  type t

  val read_line : t -> string option Io.t

  val read_exactly : t -> int -> string option Io.t

  val write : t -> string -> unit Io.t
end) : sig
  val read : Chan.t -> Jsonrpc.packet option Io.t

  val write : Chan.t -> Jsonrpc.packet -> unit Io.t
end
