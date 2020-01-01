open Import

(** * This encodes LSP RPC state machine. *)

module Server_notification : sig
  open Protocol

  type t = PublishDiagnostics of PublishDiagnostics.publishDiagnosticsParams
end

module Client_notification : sig
  open Protocol

  type t =
    | TextDocumentDidOpen of DidOpen.params
    | TextDocumentDidChange of DidChangeTextDocumentParams.t
    | Initialized
    | Exit
    | UnknownNotification of string * json option
end

type t

type 'state handler =
  { on_initialize :
         t
      -> 'state
      -> Protocol.Initialize.params
      -> ('state * Protocol.Initialize.result, string) result
  ; on_request :
      'res.    t -> 'state -> Protocol.Initialize.client_capabilities
      -> 'res Request.t -> ('state * 'res, string) result
  ; on_notification :
      t -> 'state -> Client_notification.t -> ('state, string) result
  }

val start : 'state -> 'state handler -> in_channel -> out_channel -> unit

val stop : t -> unit

val send_notification : t -> Server_notification.t -> unit
