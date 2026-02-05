open Import

let capability = "handleSwitchImplIntf", `Bool true
let meth = "ocamllsp/switchImplIntf"

(** see the spec for [ocamllsp/switchImplIntf] *)
let switch merlin_doc (param : DocumentUri.t) : Json.t =
  let files_to_switch_to = Document.get_impl_intf_counterparts merlin_doc param in
  Json.yojson_of_list Uri.yojson_of_t files_to_switch_to
;;

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  let uri = Request_uri_params.parse_exn params in
  let doc = Document_store.get_opt state.store uri in
  match doc with
  | Some doc ->
    (match Document.kind doc with
     | `Merlin merlin_doc -> switch (Some merlin_doc) uri
     | `Other ->
       Jsonrpc.Response.Error.raise
         (Jsonrpc.Response.Error.make
            ~code:InvalidRequest
            ~message:"Document with this URI is not supported by ocamllsp/switchImplIntf"
            ~data:(Uri.yojson_of_t uri)
            ()))
  | None -> switch None uri
;;
