open Import

let action = "ineferred_intf"

let infer_intf impl =
  Document.with_pipeline impl (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      let sig_ : Types.signature =
        let typedtree = Mtyper.get_typedtree typer in
        match typedtree with
        | `Interface _ -> assert false
        | `Implementation impl -> impl.str_type
      in
      Format.asprintf "%a@." Printtyp.signature sig_)

let code_action_of_intf uri intf =
  let range = Range.create ~start:Position.start ~end_:Position.start in
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range; newText = intf } in
    let uri = Uri.to_string uri in
    WorkspaceEdit.create ~changes:[ (uri, [ textedit ]) ] ()
  in
  let title = String.capitalize_ascii "Insert inteferred interface" in
  CodeAction.create ~title ~kind:(CodeActionKind.Other action) ~edit
    ~isPreferred:false ()

let code_action doc store =
  let intf_uri = Document.uri doc in
  let impl_uri = intf_uri in
  match Document_store.get_opt store impl_uri with
  | None -> Fiber.return (Ok None)
  | Some impl -> (
    let open Fiber.O in
    let+ intf = infer_intf impl in
    match intf with
    | Error e -> Error (Jsonrpc.Response.Error.of_exn e)
    | Ok intf -> Ok (Some (code_action_of_intf intf_uri intf)) )
