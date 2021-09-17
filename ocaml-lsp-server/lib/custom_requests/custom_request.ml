open Import

type 't req_params_spec =
  { params_schema : Jsonrpc.Message.Structured.t
  ; of_jsonrpc_params : Jsonrpc.Message.Structured.t -> 't option
  }

let of_jsonrpc_params_exn spec params =
  let raise_invalid_params ?data ~message () =
    Jsonrpc.Response.Error.raise
    @@ Jsonrpc.Response.Error.make ?data
         ~code:Jsonrpc.Response.Error.Code.InvalidParams ~message ()
  in
  match params with
  | None -> raise_invalid_params ~message:"Expected params but received none" ()
  | Some params -> (
    match spec.of_jsonrpc_params params with
    | Some t -> t
    | None ->
      let error_json =
        `Assoc
          [ ("params_expected", (spec.params_schema :> Json.t))
          ; ("params_received", (params :> Json.t))
          ]
      in
      raise_invalid_params ~message:"Unexpected parameter format"
        ~data:error_json ())
