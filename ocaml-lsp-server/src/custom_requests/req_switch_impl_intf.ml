open Import

let capability = "handleSwitchImplIntf", `Bool true
let meth = "ocamllsp/switchImplIntf"

(** see the spec for [ocamllsp/switchImplIntf] *)
let switch merlin_doc (param : DocumentUri.t) : Json.t =
  let files_to_switch_to = Document.get_impl_intf_counterparts merlin_doc param in
  Json.yojson_of_list Uri.yojson_of_t files_to_switch_to
;;

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  match params with
  | Some (`List [ json_uri ]) ->
    let uri = DocumentUri.t_of_yojson json_uri in
    (match Document_store.get_opt state.store uri with
     | Some doc ->
       (match Document.kind doc with
        | `Merlin merlin_doc -> switch (Some merlin_doc) uri
        | `Other ->
          Jsonrpc.Response.Error.raise
            (Jsonrpc.Response.Error.make
               ~code:InvalidRequest
               ~message:
                 "Document with this URI is not supported by ocamllsp/switchImplIntf"
               ~data:(`Assoc [ "param", (json_uri :> Json.t) ])
               ()))
     | None -> switch None uri)
  | Some json ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidRequest
         ~message:"The input parameter for ocamllsp/switchImplIntf is invalid"
         ~data:(`Assoc [ "param", (json :> Json.t) ])
         ())
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidRequest
         ~message:"ocamllsp/switchImplIntf must receive param: DocumentUri.t"
         ())
;;
