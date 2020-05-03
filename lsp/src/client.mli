open Import
open Types

type t

type handler =
  { on_request :
      'res.    t -> 'res Server_request.t
      -> ('res, Jsonrpc.Response.Error.t) result Fiber.t
  ; on_notification : t -> Server_notification.t -> unit
  }

val stop : t -> unit Fiber.t

val create : handler -> in_channel -> out_channel -> InitializeParams.t -> t

val start : t -> unit Fiber.t

val initialized : t -> InitializeResult.t Fiber.t

val send_request : t -> 'a Client_request.t -> 'a Fiber.t

val send_notification : t -> Client_notification.t -> unit
