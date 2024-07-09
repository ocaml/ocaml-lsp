open Import
open Fiber.O
module Printtyp = Merlin_analysis.Type_utils.Printtyp

let get_typer doc =
  Document.Merlin.with_pipeline_exn ~name:"infer interface" doc (fun pipeline ->
    Mpipeline.typer_result pipeline)
;;

let get_doc_signature typer =
  let typedtree = Mtyper.get_typedtree typer in
  match typedtree with
  | `Interface intf -> intf.sig_type
  | `Implementation impl -> impl.str_type
;;

(** Called by the code action for insert-interface. *)
let infer_missing_intf_for_impl impl_doc intf_doc =
  match Document.kind impl_doc, Document.kind intf_doc with
  | `Merlin impl, `Merlin intf
    when Document.Merlin.kind impl = Impl && Document.Merlin.kind intf = Intf ->
    let drop_existing_ids full_sig cur_sig =
      let existing_ids = List.map cur_sig ~f:Types.signature_item_id in
      List.filter
        ~f:(fun si ->
          let id = Types.signature_item_id si in
          not (List.mem existing_ids id ~equal:Ident.equal))
        full_sig
    in
    let* typers = Fiber.parallel_map ~f:get_typer [ impl; intf ] in
    (match typers with
     | [ impl_typer; intf_typer ] ->
       let full_sig = get_doc_signature impl_typer in
       let cur_sig = get_doc_signature intf_typer in
       let sig_update = drop_existing_ids full_sig cur_sig in
       let env = Mtyper.initial_env impl_typer in
       let* intf_cfg = Document.Merlin.mconfig intf in
       let verbosity = intf_cfg.query.verbosity in
       Printtyp.wrap_printing_env ~verbosity env (fun () ->
         Format.asprintf "%a@." Printtyp.signature sig_update)
       |> Fiber.return
     | _ -> Code_error.raise "promblem encountered with Merlin typer_result" [])
  | _ -> Code_error.raise "expected implementation and interface documents" []
;;

(* No longer involved in the insert-interface code action, but still used by the
   [ocamllsp/inferIntf] custom request. *)
let infer_intf_for_impl doc =
  match Document.kind doc with
  | `Other ->
    Code_error.raise "expected an implementation document, got a non merlin document" []
  | `Merlin m when Document.Merlin.kind m = Intf ->
    Code_error.raise "expected an implementation document, got an interface instead" []
  | `Merlin doc ->
    Document.Merlin.with_pipeline_exn ~name:"infer-interface" doc (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      let sig_ : Types.signature =
        let typedtree = Mtyper.get_typedtree typer in
        match typedtree with
        | `Interface _ -> assert false
        | `Implementation doc -> doc.str_type
      in
      let env = Mtyper.initial_env typer in
      let verbosity = (Mpipeline.final_config pipeline).query.verbosity in
      Printtyp.wrap_printing_env ~verbosity env (fun () ->
        Format.asprintf "%a@." Printtyp.signature sig_))
;;

let language_id_of_fname s =
  match Filename.extension s with
  | ".mli" | ".eliomi" -> "ocaml.interface"
  | ".ml" | ".eliom" -> "ocaml"
  | ".rei" | ".re" -> "reason"
  | ".mll" -> "ocaml.ocamllex"
  | ".mly" -> "ocaml.menhir"
  | ext -> Code_error.raise "unsupported file extension" [ "extension", String ext ]
;;

let open_document_from_file (state : State.t) uri =
  let filename = Uri.to_path uri in
  Fiber.of_thunk (fun () ->
    match Io.String_path.read_file filename with
    | exception Sys_error _ ->
      Log.log ~section:"debug" (fun () ->
        Log.msg "Unable to open file" [ "filename", `String filename ]);
      Fiber.return None
    | text ->
      let languageId = language_id_of_fname filename in
      let text_document = TextDocumentItem.create ~uri ~languageId ~version:0 ~text in
      let params = DidOpenTextDocumentParams.create ~textDocument:text_document in
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
;;

let infer_intf (state : State.t) intf_doc =
  match Document.kind intf_doc with
  | `Other -> Code_error.raise "the provided document is not a merlin source." []
  | `Merlin m when Document.Merlin.kind m = Impl ->
    Code_error.raise "the provided document is not an interface." []
  | `Merlin m ->
    Fiber.of_thunk (fun () ->
      let intf_uri = Document.uri intf_doc in
      let impl_uri =
        Document.get_impl_intf_counterparts (Some m) intf_uri |> List.hd_exn
      in
      let* impl_opt =
        match Document_store.get_opt state.store impl_uri with
        | Some impl -> Fiber.return (Some impl)
        | None -> open_document_from_file state impl_uri
      in
      match impl_opt with
      | None -> Fiber.return None
      | Some impl_doc ->
        let+ res = infer_missing_intf_for_impl impl_doc intf_doc in
        Some res)
;;

(** Extracts an [Ident.t] from all variants that have one at the top level. For
    many of the other variants, it would be possible to extract a list of IDs,
    but that's not needed for the update-signatures code action. *)
let top_level_id (item : Typedtree.signature_item) =
  match item.sig_desc with
  | Typedtree.Tsig_value { val_id; _ } -> Some val_id
  | Typedtree.Tsig_module { md_id; _ } -> md_id
  | Typedtree.Tsig_modsubst { ms_id; _ } -> Some ms_id
  | Typedtree.Tsig_modtype { mtd_id; _ } -> Some mtd_id
  | Typedtree.Tsig_modtypesubst { mtd_id; _ } -> Some mtd_id
  | Typedtree.Tsig_type _
  | Typedtree.Tsig_typesubst _
  | Typedtree.Tsig_typext _
  | Typedtree.Tsig_exception _
  | Typedtree.Tsig_recmodule _
  | Typedtree.Tsig_open _
  | Typedtree.Tsig_include _
  | Typedtree.Tsig_class _
  | Typedtree.Tsig_class_type _
  | Typedtree.Tsig_attribute _ -> None
;;

(** Represents an item that's present in the existing interface and has a
    (possibly differing) signature inferred from the implementation. *)
type shared_signature =
  { range : Range.t (* location in the interface *)
  ; old_sig : Types.signature_item (* found in the interface *)
  ; new_sig : Types.signature_item (* inferred from the implementation *)
  }

(** Try to make a [shared_signature], if an ID can be extracted from the
    [tree_item] and a matching ID can be found in both signature lists. *)
let find_shared_signature tree_item ~old_sigs ~new_sigs =
  let open Option.O in
  let* id = top_level_id tree_item in
  let id_equal sig_item = Ident.equal id (Types.signature_item_id sig_item) in
  let* old_sig = List.find ~f:id_equal old_sigs in
  let* new_sig = List.find ~f:id_equal new_sigs in
  let range = Range.of_loc tree_item.sig_loc in
  Some { range; old_sig; new_sig }
;;

(** Slices out the signatures between [first] and [last] to speed up future
    searches. This assumes that [first] and [last] came from the [sig_items]
    field on a [Typedtree.signature], and [sig_type_list] is the [sig_type]
    field on the same [Typedtree.signature], meaning that the lists will be in
    the same order. *)
let select_matching_range ~first ~last sig_type_list =
  let index_of item =
    let open Option.O in
    let* item in
    let* id = top_level_id item in
    let* i, _ =
      List.findi sig_type_list ~f:(fun _ item ->
        Ident.equal id (Types.signature_item_id item))
    in
    Some i
  in
  let start_index = index_of first |> Option.value ~default:0 in
  let end_index =
    index_of last |> Option.value ~default:(List.length sig_type_list - 1)
  in
  List.sub sig_type_list ~pos:start_index ~len:(end_index + 1 - start_index)
;;

(** Formats both the old and new signatures as they would appear in the
    interface. If they differ, create a text edit that updates to the new
    signature. *)
let text_edit_opt shared_signature ~formatter =
  (* CR-someday bwiedenbeck: We're relying on string equivalence of how the two signatures
     are printed to decide if there's been an update. It'd be nice to check some sort of
     logical equivalence on the actual types and then only format the ones that differ,
     but that's not practical with the type information we have easy access to. *)
  let+ sig_strings =
    Fiber.parallel_map ~f:formatter [ shared_signature.old_sig; shared_signature.new_sig ]
  in
  match sig_strings with
  | [ oldText; newText ] when not (String.equal oldText newText) ->
    Some ({ range = shared_signature.range; newText } : TextEdit.t)
  | _ -> None
;;

(** Produces text edits for every signature where the [formatter] produces a
    different string on the [signature_item]s from the old interface and the new
    implementation. *)
let build_signature_edits
  ~(old_intf : Typedtree.signature)
  ~(* Extracted by Merlin from the interface. *)
  (range : Range.t)
  ~(* Selected range in the interface. *)
  (new_sigs : Types.signature)
  ~(* Inferred by Merlin from the implementation. *)
  (formatter : Types.signature_item -> string Fiber.t)
  =
  (* These are [Typedtree.signature_item]s, and we need them for the location. *)
  let in_range_tree_items =
    List.filter old_intf.sig_items ~f:(fun si ->
      Range.overlaps range (Range.of_loc si.sig_loc))
  in
  let first = List.hd in_range_tree_items in
  let last = List.last in_range_tree_items in
  (* These are [Types.signature_item]s, and we need them to match up types. *)
  let in_range_old_sigs = select_matching_range ~first ~last old_intf.sig_type in
  let in_range_new_sigs =
    (* This list can be big and we might search it many times when finding
       [shared_signatures], so it's worth doing a scan that shrinks it. *)
    List.filter new_sigs ~f:(fun si ->
      let in_range_old_ids = List.map in_range_old_sigs ~f:Types.signature_item_id in
      let id = Types.signature_item_id si in
      List.mem in_range_old_ids id ~equal:Ident.equal)
  in
  let shared_signatures =
    List.filter_map
      in_range_tree_items
      ~f:(find_shared_signature ~old_sigs:in_range_old_sigs ~new_sigs:in_range_new_sigs)
  in
  let+ updates = Fiber.parallel_map shared_signatures ~f:(text_edit_opt ~formatter) in
  List.filter_opt updates
;;

(** Called by the code action for update-signatures. *)
let update_signatures
  ~(state : State.t)
  ~(intf_merlin : Document.Merlin.t)
  ~(doc : Document.t)
  ~(range : Range.t)
  =
  Fiber.of_thunk (fun () ->
    let intf_uri = Document.uri doc in
    let impl_uri =
      Document.get_impl_intf_counterparts (Some intf_merlin) intf_uri |> List.hd_exn
    in
    let* impl_doc =
      match Document_store.get_opt state.store impl_uri with
      | Some impl -> Fiber.return (Some impl)
      | None -> open_document_from_file state impl_uri
    in
    match impl_doc with
    | None -> Fiber.return []
    | Some impl_doc ->
      let impl_merlin = Document.merlin_exn impl_doc in
      (* CR-someday bwiedenbeck: These calls to Merlin to get the type information (and
         the subsequent processing we do with it) are expensive on large documents.
         This can cause problems if someone is trying to invoke some other code action,
         because the LSP currently determines which CAs are possible by trying them all.
         We've decided for now to allow slow code actions (especially since users are
         less likely to be doing lots of little CAs in the mli file) and think more
         about the broader CA protocol in the future. *)
      let* typers = Fiber.parallel_map [ intf_merlin; impl_merlin ] ~f:get_typer in
      let intf_typer = List.hd_exn typers in
      let impl_typer = List.nth_exn typers 1 in
      (match Mtyper.get_typedtree intf_typer with
       | `Interface old_intf ->
         let formatter sig_item =
           let* config = Document.Merlin.mconfig intf_merlin in
           let verbosity = config.query.verbosity in
           let env = Mtyper.initial_env intf_typer in
           Fiber.return
             (Printtyp.wrap_printing_env ~verbosity env (fun () ->
                Format.asprintf "%a@." Printtyp.signature [ sig_item ]))
         in
         let new_sigs = get_doc_signature impl_typer in
         build_signature_edits ~old_intf ~new_sigs ~range ~formatter
       | _ -> Code_error.raise "expected an interface" []))
;;
