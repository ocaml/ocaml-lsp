module Events : sig
  type t

  val create : max_n_last_events:int -> t

  val add : t -> Chrome_trace.Event.t -> unit

  val to_list : t -> Chrome_trace.Event.t list
end = struct
  type t =
    { events : Chrome_trace.Event.t Dlist.t
    ; max_n_last_events : int
    }

  let create ~max_n_last_events =
    { events = Dlist.create (); max_n_last_events }

  let add t event =
    if Dlist.length t.events + 1 > t.max_n_last_events then
      Dlist.drop_one_right_exn t.events;
    Dlist.prepend t.events event

  let to_list t = Dlist.to_list t.events
end

(** a server processes requests in several layers: jsonrpc -> lsp ->
    application; this library allows to enable tracing in certain layers and
    disable in others *)
type trace_layers =
  { app : bool
  ; lsp : bool
  ; jsonrpc : bool
  }

let trace_layers ?app ?lsp ?jsonrpc () =
  (* TODO: should we assert that at least one is not None? *)
  let app = Option.is_some app in
  let lsp = Option.is_some lsp in
  let jsonrpc = Option.is_some jsonrpc in
  { app; lsp; jsonrpc }

type t =
  { events : Events.t
  ; layers : trace_layers
  }

let create ~max_n_last_events layers =
  { events = Events.create ~max_n_last_events; layers }

type jsonrpc_id =
  [ `String of string
  | `Int of int
  ]

let event_id_of_jsonrpc_id = function
  | `String s -> Chrome_trace.Event.Id.String s
  | `Int i -> Int i

let should_capture_req { layers = { lsp; jsonrpc; _ }; _ }
    ~(me : [ `App | `Lsp ]) =
  match me with
  | `App -> lsp || jsonrpc
  | `Lsp -> jsonrpc

let req_resp_event_common_fields ~name =
  Chrome_trace.Event.common_fields
    ~cat:[ "client_req-server_resp" ]
    ~tid:(Thread.id @@ Thread.self ())
    ~ts:(Chrome_trace.Event.Timestamp.now ())
    ~name ()

let trace_lsp_recv_req ?name tracer_opt ~(id : jsonrpc_id) ~(meth : string)
    ~params : unit =
  let trace tracer =
    let name = Option.value name ~default:meth in
    let common_fields = req_resp_event_common_fields ~name in
    let id = event_id_of_jsonrpc_id id in
    let args =
      if should_capture_req tracer ~me:`Lsp then
        []
      else
        let params = Option.value params ~default:(`Assoc []) in
        [ ("request", `String (Yojson.Safe.pretty_to_string params)) ]
    in
    let async_event =
      Chrome_trace.Event.async id Chrome_trace.Event.Start common_fields ~args
    in
    Events.add tracer.events async_event
  in
  Option.iter trace tracer_opt

let trace_lsp_send_resp ?name tracer_opt ~(id : jsonrpc_id) ~(meth : string)
    ~resp : unit =
  let trace tracer =
    let name = Option.value name ~default:meth in
    let common_fields = req_resp_event_common_fields ~name in
    let id = event_id_of_jsonrpc_id id in
    (* TODO: use layers *)
    let args = [ ("response", `String (Yojson.Safe.pretty_to_string resp)) ] in
    let async_event =
      Chrome_trace.Event.async id Chrome_trace.Event.End common_fields ~args
    in
    Events.add tracer.events async_event
  in
  Option.iter trace tracer_opt

let events t = Events.to_list t.events
