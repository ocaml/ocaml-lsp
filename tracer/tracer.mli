type t

type trace_layers =
  { app : bool
  ; lsp : bool
  ; jsonrpc : bool
  }

val trace_layers :
  ?app:unit -> ?lsp:unit -> ?jsonrpc:unit -> unit -> trace_layers

val create : max_n_last_events:int -> trace_layers -> t

val trace_lsp_recv_req :
     ?name:string
  -> t option
  -> id:[ `String of string | `Int of int ]
  -> meth:string
  -> params:Yojson.Safe.t option
  -> unit

val trace_lsp_send_resp :
     ?name:string
  -> t option
  -> id:[ `String of string | `Int of int ]
  -> meth:string
  -> resp:Yojson.Safe.t
  -> unit

val events : t -> Chrome_trace.Event.t list
