open Import
open Fiber.O

let capability = "handleInferIntf", `Bool true
let meth = "ocamllsp/inferIntf"

let get_doc_id ~(params : Jsonrpc.Structured.t option) =
  match params with
  | Some (`List [ json_uri ]) ->
    let uri = DocumentUri.t_of_yojson json_uri in
    Some { TextDocumentIdentifier.uri }
  | _ -> None
;;

let on_request ~log_info ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    match params with
    | Some (`List [ json_uri ]) ->
      let json_uri = DocumentUri.t_of_yojson json_uri in
      (match Document_store.get_opt state.store json_uri with
       | None ->
         Jsonrpc.Response.Error.raise
           (Jsonrpc.Response.Error.make
              ~code:InvalidParams
              ~message:
                "ocamllsp/inferIntf received a URI for an unloaded file. Load the file \
                 first."
              ())
       | Some impl ->
         let+ intf = Inference.infer_intf_for_impl ~log_info impl in
         Json.t_of_yojson (`String intf))
    | Some json ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidRequest
           ~message:"The input parameter for ocamllsp/inferIntf is invalid"
           ~data:(`Assoc [ "param", (json :> Json.t) ])
           ())
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidRequest
           ~message:"ocamllsp/inferIntf must receive param: DocumentUri.t"
           ()))
;;
