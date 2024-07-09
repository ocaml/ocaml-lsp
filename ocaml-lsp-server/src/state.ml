open Import

type init =
  | Uninitialized
  | Initialized of
      { params : InitializeParams.t
      ; workspaces : Workspaces.t
      ; dune : Dune.t
      ; exp_client_caps : Client.Experimental_capabilities.t
      ; diagnostics : Diagnostics.t
      ; position_encoding : [ `UTF16 | `UTF8 ]
      }

type hover_extended = { mutable history : (Uri.t * Position.t * int) option }

type t =
  { store : Document_store.t
  ; merlin : Document.Single_pipeline.t
  ; merlin_config : Merlin_config.DB.t
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; symbols_thread : Lev_fiber.Thread.t Lazy_fiber.t
  ; wheel : Lev_fiber.Timer.Wheel.t
  ; hover_extended : hover_extended
  }

let create ~store ~merlin ~detached ~configuration ~ocamlformat_rpc ~symbols_thread ~wheel
  =
  { init = Uninitialized
  ; merlin_config = Merlin_config.DB.create ()
  ; store
  ; merlin = Document.Single_pipeline.create merlin
  ; detached
  ; configuration
  ; trace = Off
  ; ocamlformat_rpc
  ; symbols_thread
  ; wheel
  ; hover_extended = { history = None }
  }
;;

let wheel t = t.wheel

let initialize_params (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized init -> init.params
;;

let workspaces (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized init -> init.workspaces
;;

let workspace_root t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init ->
    (match init.params.rootUri with
     | None -> assert false
     | Some uri -> uri)
;;

let dune t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.dune
;;

let position_encoding t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.position_encoding
;;

let diagnostics t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.diagnostics
;;

let initialize
  t
  ~position_encoding
  (params : InitializeParams.t)
  workspaces
  dune
  diagnostics
  =
  assert (t.init = Uninitialized);
  { t with
    init =
      Initialized
        { params
        ; workspaces
        ; dune
        ; diagnostics
        ; position_encoding
        ; exp_client_caps =
            Client.Experimental_capabilities.of_opt_json params.capabilities.experimental
        }
  }
;;

let modify_workspaces t ~f =
  let init =
    match t.init with
    | Uninitialized -> assert false
    | Initialized init -> Initialized { init with workspaces = f init.workspaces }
  in
  { t with init }
;;

let client_capabilities t = (initialize_params t).capabilities

let experimental_client_capabilities t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized { exp_client_caps; _ } -> exp_client_caps
;;

let log_msg server ~type_ ~message =
  let state = Server.state server in
  task_if_running state.detached ~f:(fun () ->
    let log = LogMessageParams.create ~type_ ~message in
    Server.notification server (Server_notification.LogMessage log))
;;
