type t

val create : keep:[ `All_events | `Last_n_events of int ] -> t

val to_list : t -> Chrome_trace.Event.t list

module Lsp : sig
  val trace_recv_req : t option -> Jsonrpc.Id.t Jsonrpc.Message.t -> unit

  val trace_send_resp : t option -> method_:string -> Jsonrpc.Response.t -> unit
end
