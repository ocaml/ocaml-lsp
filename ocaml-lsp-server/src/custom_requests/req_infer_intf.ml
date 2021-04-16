open Import

let capability = ("handleInferIntf", `Bool true)

let meth = "ocamllsp/inferIntf"

let on_request ~(params : Jsonrpc.Message.Structured.t option) (state : State.t)
    =
  match params with
  | Some (`List [ json_uri ]) -> (
    let json_uri = DocumentUri.t_of_yojson json_uri in
    let open Fiber.O in
    match Document_store.get_opt state.store json_uri with
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidParams
           ~message:
             "ocamllsp/inferIntf received a URI for an unloaded file. Load the \
              file first."
           ())
    | Some impl ->
      let+ intf = Inference.infer_intf_for_impl impl in
      Json.t_of_yojson (`String intf))
  | Some json ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:"The input parameter for ocamllsp/inferIntf is invalid"
         ~data:(`Assoc [ ("param", (json :> Json.t)) ])
         ())
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:"ocamllsp/inferIntf must receive param: DocumentUri.t" ())
