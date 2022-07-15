(** Canonical S-expression RPC.

    This module implements a RPC mechanism for exchanging canonical
    S-expressions over unix or internet sockets. It allows a server to accept
    connections and a client to connect to a server.

    However, it doesn't explain how to encode queries, responses or generally
    any kind of messages as Canonical S-expressions. This part should be built
    on top of this module. *)

module Session : sig
  type t
  (** Rpc session backed by an input & output stream *)

  val create :
    socket:bool ->
    Lev_fiber.Io.input Lev_fiber.Io.t ->
    Lev_fiber.Io.output Lev_fiber.Io.t ->
    t

  val close : t -> unit

  (* [write t x] writes the s-expression when [x] is [Some sexp], and closes the
     session if [x = None ] *)
  val write : t -> Csexp.t list -> unit Fiber.t

  val read : t -> Csexp.t option Fiber.t
  (** If [read] returns [None], the session is closed and all subsequent reads
      will return [None] *)
end

val connect : Lev_fiber.Fd.t -> Unix.sockaddr -> Session.t Fiber.t

val serve :
  Lev_fiber.Socket.Server.t -> f:(Session.t -> unit Fiber.t) -> unit Fiber.t
