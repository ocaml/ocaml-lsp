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
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; diagnostics : Diagnostics.t
  ; symbols_thread : Scheduler.thread Lazy_fiber.t
  }

(** @raise Assertion_failure if server is uninitialized *)
let initialization_params t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized params -> params

let workspace_root t =
  let { InitializeParams.rootUri; _ } = initialization_params t in
  match rootUri with
  | None -> assert false
  | Some uri -> uri

