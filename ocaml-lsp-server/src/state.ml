open Import

type init =
  | Uninitialized
  | Initialized of InitializeParams.t

module Uri_map = Map.Make (struct
  include Uri

  let compare x y = Ordering.of_int (compare x y)
end)

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; workspace_folders : WorkspaceFolder.t Uri_map.t option
  ; trace : TraceValue.t
  ; ocamlformat : Fmt.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
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
