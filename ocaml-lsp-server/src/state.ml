open Import

type init =
  | Uninitialized
  | Initialized of InitializeParams.t * Workspaces.t

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat : Fmt.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; diagnostics : Diagnostics.t
  ; symbols_thread : Scheduler.thread Lazy_fiber.t
  }

let initialize_params (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized (init, _) -> init

let workspaces (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized (_, ws) -> ws

let workspace_root t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized (i, _) -> (
    match i.rootUri with
    | None -> assert false
    | Some uri -> uri)

let initialize t ip =
  assert (t.init = Uninitialized);
  let ws = Workspaces.create ip in
  { t with init = Initialized (ip, ws) }

let modify_workspaces t ~f =
  let init =
    match t.init with
    | Uninitialized -> assert false
    | Initialized (i, ws) -> Initialized (i, f ws)
  in
  { t with init }
