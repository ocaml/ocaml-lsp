open Import

module Initialization_options = struct
  type merlin_construct_options = { use_local_context : bool }

  (** [relativePath] is a path relative to obj [initializationOptions] *)
  let raise_incorrect_json ?relative_path () =
    let relative_path_s =
      match relative_path with
      | None -> ""
      | Some s -> "." ^ s
    in
    Code_error.raise
      (sprintf "client is sending incorrect `initializationOptions%s` object"
         relative_path_s)
      []

  let merlin_construct_options_of_yojson = function
    | `Assoc [ ("use_local_context", `Bool b) ] -> { use_local_context = b }
    | _ -> raise_incorrect_json ~relative_path:"construct" ()

  type t = { construct : merlin_construct_options }

  let default : t = { construct = { use_local_context = false } }

  let t_of_yojson = function
    | `Assoc [ ("construct", construct_json) ] ->
      let construct = merlin_construct_options_of_yojson construct_json in
      { construct }
    | _ -> raise_incorrect_json ()
end

module Initialize_params = Initialize_params (Initialization_options)

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
