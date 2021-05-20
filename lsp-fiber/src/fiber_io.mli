(** Reprsents a bi-directional jsonrpc packet stream read in dedicated threads.

    TODO Nothing here is specific to jsonrpc *)
open! Import

type t

val close : t -> [ `Read | `Write ] -> unit Fiber.t

val send : t -> Jsonrpc.packet list -> unit Fiber.t

val recv : t -> Jsonrpc.packet option Fiber.t

val make : Io.t -> t Fiber.t
