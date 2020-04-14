open! Import
open Types

(** * This encodes LSP RPC state machine. *)

type t

type 'state handler =
  { on_initialize :
         t
      -> 'state
      -> InitializeParams.t
      -> ('state * InitializeResult.t, string) result
  ; on_request :
      'res.    t -> 'state -> ClientCapabilities.t -> 'res Client_request.t
      -> ('state * 'res, Jsonrpc.Response.Error.t) result
  ; on_notification :
      t -> 'state -> Client_notification.t -> ('state, string) result
  }

val start :
  'state -> 'state handler -> in_channel -> out_channel -> unit Fiber.t

val stop : t -> unit

val send_notification : t -> Server_notification.t -> unit
