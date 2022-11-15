open Import
open Fiber.O

let infer_intf_for_impl doc =
  match Document.kind doc with
  | `Other ->
    Code_error.raise
      "expected an implementation document, got a non merlin document"
      []
  | `Merlin m when Document.Merlin.kind m = Intf ->
    Code_error.raise
      "expected an implementation document, got an interface instead"
      []
  | `Merlin doc ->
    Document.Merlin.with_pipeline_exn doc (fun pipeline ->
        let typer = Mpipeline.typer_result pipeline in
        let sig_ : Types.signature =
          let typedtree = Mtyper.get_typedtree typer in
          match typedtree with
          | `Interface _ -> assert false
          | `Implementation doc -> doc.str_type
        in
        let env = Mtyper.initial_env typer in
        let verbosity = (Mpipeline.final_config pipeline).query.verbosity in
        let module Printtyp = Merlin_analysis.Type_utils.Printtyp in
        Printtyp.wrap_printing_env ~verbosity env (fun () ->
            Format.asprintf "%a@." Printtyp.signature sig_))

let language_id_of_fname s =
  match Filename.extension s with
  | ".mli" | ".eliomi" -> "ocaml.interface"
  | ".ml" | ".eliom" -> "ocaml"
  | ".rei" | ".re" -> "reason"
  | ".mll" -> "ocaml.ocamllex"
  | ".mly" -> "ocaml.menhir"
  | ext ->
    Code_error.raise "unsupported file extension" [ ("extension", String ext) ]

let open_document_from_file (state : State.t) uri =
  let filename = Uri.to_path uri in
  Fiber.of_thunk (fun () ->
      match Io.String_path.read_file filename with
      | exception Sys_error _ ->
        Log.log ~section:"debug" (fun () ->
            Log.msg "Unable to open file" [ ("filename", `String filename) ]);
        Fiber.return None
      | text ->
        let languageId = language_id_of_fname filename in
        let text_document =
          TextDocumentItem.create ~uri ~languageId ~version:0 ~text
        in
        let params =
          DidOpenTextDocumentParams.create ~textDocument:text_document
        in
        let+ doc =
          let position_encoding = State.position_encoding state in
          Document.make
            ~position_encoding
            (State.wheel state)
            state.merlin_config
            state.merlin
            params
        in
        Some doc)

let infer_intf (state : State.t) doc =
  match Document.kind doc with
  | `Other ->
    Code_error.raise "the provided document is not a merlin source." []
  | `Merlin m when Document.Merlin.kind m = Impl ->
    Code_error.raise "the provided document is not an interface." []
  | `Merlin _ ->
    Fiber.of_thunk (fun () ->
        let intf_uri = Document.uri doc in
        let impl_uri =
          Document.get_impl_intf_counterparts intf_uri |> List.hd
        in
        let* impl =
          match Document_store.get_opt state.store impl_uri with
          | Some impl -> Fiber.return (Some impl)
          | None -> open_document_from_file state impl_uri
        in
        match impl with
        | None -> Fiber.return None
        | Some impl ->
          let+ res = infer_intf_for_impl impl in
          Some res)
