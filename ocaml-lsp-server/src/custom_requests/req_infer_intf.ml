open Import

let capability = ("handleInferIntf", `Bool true)

let meth = "ocamllsp/inferIntf"

let on_request ~(params : Json.t option) (state : State.t) =
  match params with
  | Some (`String (_ : DocumentUri.t) as json_uri) -> (
    let open Fiber.O in
    match Document_store.get_opt state.store (Uri.t_of_yojson json_uri) with
    | None ->
      Fiber.return
      @@ Error
           (Jsonrpc.Response.Error.make ~code:InvalidParams
              ~message:
                "ocamllsp/inferIntf received a URI for an unloaded file. Load \
                 the file first."
              ())
    | Some impl -> (
      let+ intf = Inference.infer_intf_for_impl impl in
      match intf with
      | Error e -> Error (Jsonrpc.Response.Error.of_exn e)
      | Ok intf -> Ok (Json.t_of_yojson (`String intf)) ) )
  | Some _
  | None ->
    Fiber.return
    @@ Error
         (Jsonrpc.Response.Error.make ~code:InvalidRequest
            ~message:"ocamllsp/inferIntf must receive param : DocumentUri.t" ())
