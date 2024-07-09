(** Reprsents a bi-directional jsonrpc packet stream read in dedicated threads.

    TODO Nothing here is specific to jsonrpc *)
open! Import

type t

val close : t -> [ `Read | `Write ] -> unit Fiber.t
val send : t -> Jsonrpc.Packet.t list -> unit Fiber.t
val recv : t -> Jsonrpc.Packet.t option Fiber.t
val make : Lev_fiber.Io.input Lev_fiber.Io.t -> Lev_fiber.Io.output Lev_fiber.Io.t -> t
