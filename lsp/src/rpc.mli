open! Import

(** * This encodes LSP RPC state machine. *)

type t

type 'state handler =
  { on_initialize :
         t
      -> 'state
      -> Initialize.Params.t
      -> ('state * Initialize.Result.t, string) result
  ; on_request :
      'res.    t -> 'state -> Initialize.ClientCapabilities.t
      -> 'res Client_request.t -> ('state * 'res, string) result
  ; on_notification :
      t -> 'state -> Client_notification.t -> ('state, string) result
  }

val start : 'state -> 'state handler -> in_channel -> out_channel -> unit

val stop : t -> unit

val send_notification : t -> Server_notification.t -> unit
