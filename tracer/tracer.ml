module Event = Chrome_trace.Event
module Json = Chrome_trace.Json

(** A data structure to keep trace events in. It can store either unlimited or
    limited number of events. *)
module Events : sig
  type t

  val create : keep:[ `All_events | `Last_n_events of int ] -> t

  val add : t -> Event.t -> unit

  val to_list : t -> Event.t list
end = struct
  type t =
    | Limited of Event.t Ring_buf.t
    | Unlimited of Event.t list ref
        (** events are kept in rev chronological order *)

  let create ~keep =
    match keep with
    | `Last_n_events size -> Limited (Ring_buf.create ~size)
    | `All_events -> Unlimited (ref [])

  let add (t : t) event =
    match t with
    | Limited rb -> Ring_buf.push_back rb event
    | Unlimited r -> r := event :: !r

  let to_list = function
    | Limited rb -> Ring_buf.to_list rb
    | Unlimited r -> List.rev !r
end

type t = Events.t

let create ~keep = Events.create ~keep

let emit = Events.add

let to_list = Events.to_list

let _record_gc_and_fd stats =
  let module Event = Chrome_trace.Event in
  let ts = Event.Timestamp.now () in
  let common = Event.common_fields ~name:"gc" ~ts () in
  let args =
    let stat = Gc.stat () in
    [ ("live_words", `Int stat.live_words)
    ; ("free_words", `Int stat.free_words)
    ; ("stack_size", `Int stat.stack_size)
    ; ("heap_words", `Int stat.heap_words)
    ; ("top_heap_words", `Int stat.top_heap_words)
    ; ("minor_words", `Float stat.minor_words)
    ; ("major_words", `Float stat.major_words)
    ; ("promoted_words", `Float stat.promoted_words)
    ; ("compactions", `Int stat.compactions)
    ; ("major_collections", `Int stat.major_collections)
    ; ("minor_collections", `Int stat.minor_collections)
    ]
  in
  let event = Event.counter common args in
  emit stats event

(** [Lsp] is used to trace events happening in the LSP layer *)
module Lsp = struct
  let cat = [ "lsp"; "server-side"; "response latency" ]

  let this_thread () = Thread.self () |> Thread.id

  let now = Event.Timestamp.now

  let common_fields ~name =
    Event.common_fields ~cat ~tid:(this_thread ()) ~ts:(now ()) ~name ()

  let trace_recv_req tracer_opt (req : Jsonrpc.Id.t Jsonrpc.Message.t) =
    let params : Json.t =
      match req.params with
      | None -> `Null
      | Some r -> (r :> Json.t)
    in
    Option.iter
      (fun tracer ->
        let event =
          common_fields ~name:req.method_
          |> Event.async req.id Event.Start ~args:[ ("params", params) ]
        in
        emit tracer event)
      tracer_opt

  let trace_send_resp tracer_opt ~method_ (resp : Jsonrpc.Response.t) =
    let resp_payload =
      match resp.result with
      | Ok json -> json
      | Error err -> Jsonrpc.Response.Error.yojson_of_t err
    in
    Option.iter
      (fun tracer ->
        let event =
          common_fields ~name:method_
          |> Event.async resp.id Event.End ~args:[ ("response", resp_payload) ]
        in
        emit tracer event)
      tracer_opt
end
