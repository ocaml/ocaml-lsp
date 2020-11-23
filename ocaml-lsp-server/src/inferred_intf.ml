open Import

let action_kind = "inferred_intf"

let infer_intf impl =
  Document.with_pipeline impl (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      let pos = Mpipeline.get_lexing_pos pipeline `Start in
      let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
      let sig_ : Types.signature =
        let typedtree = Mtyper.get_typedtree typer in
        match typedtree with
        | `Interface _ -> assert false
        | `Implementation impl -> impl.str_type
      in
      Printtyp.wrap_printing_env env (fun () ->
          Format.asprintf "%a@." Printtyp.signature sig_))

let code_action_of_intf uri intf range =
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range; newText = intf } in
    let uri = Uri.to_string uri in
    WorkspaceEdit.create ~changes:[ (uri, [ textedit ]) ] ()
  in
  let title = String.capitalize_ascii "Insert inferred interface" in
  CodeAction.create ~title ~kind:(CodeActionKind.Other action_kind) ~edit
    ~isPreferred:false ()

let language_id_of_fname s =
  match Filename.extension s with
  | ".mli" -> "ocaml.interface"
  | ".ml" -> "ocaml"
  | ".rei"
  | ".re" ->
    "reason"
  | ".mll" -> "ocaml.ocamllex"
  | ".mly" -> "ocaml.menhir"
  | ext -> failwith ("Unknown extension " ^ ext)

let force_open_document (state : State.t) uri =
  let open Fiber.O in
  let filename = Uri.to_path uri in
  let text = Io.read_file (Fpath.of_string filename) in
  let delay = Configuration.diagnostics_delay state.configuration in
  let timer = Scheduler.create_timer state.scheduler ~delay in
  let languageId = language_id_of_fname filename in
  let text_document =
    Lsp.Types.TextDocumentItem.create ~uri:(Uri.to_string uri) ~languageId
      ~version:0 ~text
  in
  let params = DidOpenTextDocumentParams.create ~textDocument:text_document in
  let+ doc = Document.make timer state.merlin params in
  Document_store.put state.store doc;
  doc

let code_action doc (state : State.t) (params : CodeActionParams.t) =
  let open Fiber.O in
  match Document.kind doc with
  | Impl -> Fiber.return (Ok None)
  | Intf -> (
    let intf_uri = Document.uri doc in
    let intf_path = Uri.to_path intf_uri in
    let impl_path =
      Switch_impl_intf.get_intf_impl_counterparts intf_path |> List.hd
    in
    let impl_uri = Uri.of_path impl_path in
    let* impl =
      match Document_store.get_opt state.store impl_uri with
      | None -> force_open_document state impl_uri
      | Some impl -> Fiber.return impl
    in
    let+ intf = infer_intf impl in
    match intf with
    | Error e -> Error (Jsonrpc.Response.Error.of_exn e)
    | Ok intf -> Ok (Some (code_action_of_intf intf_uri intf params.range)) )
