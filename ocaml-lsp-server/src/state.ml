open Import

type init =
  | Uninitialized
  | Initialized of InitializeParams.t

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat : Fmt.t
  ; diagnostics : Diagnostics.t
  ; symbols_thread : Scheduler.thread Lazy_fiber.t
  }

let workspace_root t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized i -> (
    match i.rootUri with
    | None -> assert false
    | Some uri -> uri)
