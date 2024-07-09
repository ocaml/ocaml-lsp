(** Low level module for sending/receiving jsonrpc packets across channels *)

exception Error of string

module Make
    (Io : sig
       type 'a t

       val return : 'a -> 'a t
       val raise : exn -> 'a t

       module O : sig
         val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
         val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
       end
     end)
    (Chan : sig
       type input
       type output

       val read_line : input -> string option Io.t
       val read_exactly : input -> int -> string option Io.t
       val write : output -> string list -> unit Io.t
     end) : sig
  val read : Chan.input -> Jsonrpc.Packet.t option Io.t
  val write : Chan.output -> Jsonrpc.Packet.t -> unit Io.t
end
