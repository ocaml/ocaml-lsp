open! Import
open Jsonrpc

type t

val close : t -> unit Fiber.t

val send : t -> packet -> unit Fiber.t

val recv : t -> packet option Fiber.t

val make : Scheduler.t -> Io.t -> t
