open Import

let capability = ("handleSwitchImplIntf", `Bool true)

let meth = "ocamllsp/switchImplIntf"

(** see the spec for [ocamllsp/switchImplIntf] *)
let switch (param : DocumentUri.t) : Json.t =
  let files_to_switch_to = Document.get_impl_intf_counterparts param in
  Json.yojson_of_list Uri.yojson_of_t files_to_switch_to

let on_request ~(params : Jsonrpc.Message.Structured.t option) _ =
  Fiber.return
    (match params with
    | Some (`List [ file_uri ]) ->
      let file_uri = DocumentUri.t_of_yojson file_uri in
      switch file_uri
    | Some json ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:"The input parameter for ocamllsp/switchImplIntf is invalid"
           ~data:(`Assoc [ ("param", (json :> Json.t)) ])
           ())
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidRequest
           ~message:"ocamllsp/switchImplIntf must receive param: DocumentUri.t"
           ()))
