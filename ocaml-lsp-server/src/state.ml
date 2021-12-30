open Import

type init =
  | Uninitialized
  | Initialized of
      { params : InitializeParams.t
      ; workspaces : Workspaces.t
      ; dune : Dune.t
      }

type t =
  { store : Document_store.t
  ; merlin : Lev_fiber.Thread.t
  ; merlin_config : Merlin_config.t
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat : Ocamlformat.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; diagnostics : Diagnostics.t
  ; symbols_thread : Lev_fiber.Thread.t Lazy_fiber.t
  ; wheel : Lev_fiber.Timer.Wheel.t
  }

let create ~store ~merlin ~detached ~configuration ~ocamlformat ~ocamlformat_rpc
    ~diagnostics ~symbols_thread ~wheel =
  { init = Uninitialized
  ; merlin_config = Merlin_config.create ()
  ; store
  ; merlin
  ; detached
  ; configuration
  ; trace = `Off
  ; ocamlformat
  ; ocamlformat_rpc
  ; diagnostics
  ; symbols_thread
  ; wheel
  }

let wheel t = t.wheel

let initialize_params (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized init -> init.params

let workspaces (state : t) =
  match state.init with
  | Uninitialized -> assert false
  | Initialized init -> init.workspaces

let workspace_root t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> (
    match init.params.rootUri with
    | None -> assert false
    | Some uri -> uri)

let dune t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.dune

let initialize t params workspaces dune =
  assert (t.init = Uninitialized);
  { t with init = Initialized { params; workspaces; dune } }

let modify_workspaces t ~f =
  let init =
    match t.init with
    | Uninitialized -> assert false
    | Initialized init ->
      Initialized { init with workspaces = f init.workspaces }
  in
  { t with init }
