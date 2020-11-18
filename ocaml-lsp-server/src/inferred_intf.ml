open Import

let action_kind = "inferred_intf"

let infer_intf impl =
  Document.with_pipeline impl (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      let sig_ : Types.signature =
        let typedtree = Mtyper.get_typedtree typer in
        match typedtree with
        | `Interface _ ->
          Printf.fprintf stderr "assert false\n";
          assert false
        | `Implementation impl -> impl.str_type
      in
      Format.asprintf "%a@." Printtyp.signature sig_)

let code_action_of_intf uri intf range =
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range; newText = intf } in
    let uri = Uri.to_string uri in
    WorkspaceEdit.create ~changes:[ (uri, [ textedit ]) ] ()
  in
  let title = String.capitalize_ascii "Insert inferred interface" in
  CodeAction.create ~title ~kind:(CodeActionKind.Other action_kind) ~edit
    ~isPreferred:false ()

let code_action doc store (params : CodeActionParams.t) =
  match Document.kind doc with
  | Intf -> (
    let intf_uri = Document.uri doc in
    let intf_path = Uri.to_path intf_uri in
    let impl_path =
      Switch_impl_intf.get_intf_impl_counterparts intf_path |> List.hd
    in
    let impl_uri = Uri.of_path impl_path in
    match Document_store.get_opt store impl_uri with
    | None -> Fiber.return (Ok None)
    | Some impl -> (
      let open Fiber.O in
      let+ intf = infer_intf impl in
      match intf with
      | Error e -> Error (Jsonrpc.Response.Error.of_exn e)
      | Ok intf -> Ok (Some (code_action_of_intf intf_uri intf params.range)) )
    )
  | Impl -> Fiber.return (Ok None)
