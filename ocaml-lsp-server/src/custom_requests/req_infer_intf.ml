open Import
open Fiber.O

let capability = "handleInferIntf", `Bool true
let meth = "ocamllsp/inferIntf"

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let uri = Request_uri_params.parse_exn params in
    let doc = Document_store.get_opt state.store uri in
    match doc with
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidParams
           ~message:
             "ocamllsp/inferIntf received a URI for an unloaded file. Load the file \
              first."
           ())
    | Some impl ->
      let+ intf = Inference.infer_intf_for_impl impl in
      Json.t_of_yojson (`String intf))
;;
