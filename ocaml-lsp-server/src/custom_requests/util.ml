open Import

type 't req_params_spec =
  { params_schema : Jsonrpc.Structured.t
  ; of_jsonrpc_params : Jsonrpc.Structured.t -> 't option
  }

let with_merlin state uri ~default f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return default
  | `Merlin merlin -> f merlin
;;

let with_pipeline state uri ~default f =
  with_merlin state uri ~default (fun merlin ->
    Document.Merlin.with_pipeline_exn merlin f)
;;

let with_impl_pipeline state uri ~default f =
  with_merlin state uri ~default (fun merlin ->
    match Document.Merlin.kind merlin with
    | Document.Kind.Intf -> Fiber.return default
    | Document.Kind.Impl -> Document.Merlin.with_pipeline_exn merlin f)
;;

let raise_invalid_params ?data ~message () =
  Jsonrpc.Response.Error.raise
  @@ Jsonrpc.Response.Error.make
       ?data
       ~code:Jsonrpc.Response.Error.Code.InvalidParams
       ~message
       ()
;;

let markup_content ~kind ~value =
  let value =
    match kind with
    | MarkupKind.Markdown ->
      (match Doc_to_md.translate value with
       | Raw value | Markdown value -> value)
    | MarkupKind.PlainText -> value
  in
  MarkupContent.create ~kind ~value
;;

let of_jsonrpc_params_exn spec params =
  match params with
  | None -> raise_invalid_params ~message:"Expected params but received none" ()
  | Some params ->
    (match spec.of_jsonrpc_params params with
     | Some t -> t
     | None ->
       let error_json =
         `Assoc
           [ "params_expected", (spec.params_schema :> Json.t)
           ; "params_received", (params :> Json.t)
           ]
       in
       raise_invalid_params ~message:"Unexpected parameter format" ~data:error_json ())
;;
