open Import

let action_title = "Add missing `rec` keyword"

let let_bound_vars bindings =
  List.filter_map
    ~f:(fun vb ->
      match vb.Typedtree.vb_pat.pat_desc with
      | Typedtree.Tpat_var (id, loc) -> Some (id, loc)
      | _ -> None)
    bindings

(** If the cursor position is inside a let binding which should have a ret tag
    and does not, return the Location.t of the binding. *)
let has_missing_rec pipeline pos_start =
  let open Option.O in
  (* Find identifier under cursor *)
  let* ident =
    Compl.reconstruct_ident (Mpipeline.raw_source pipeline) pos_start
  in

  (* Find most enclosing nonrecursive let binding that binds ident *)
  let typer = Mpipeline.typer_result pipeline in
  let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
  Mbrowse.enclosing (Mpipeline.get_lexing_pos pipeline pos_start) [ browse ]
  |> List.find_map ~f:(function
       | ( _
         , Browse_raw.Structure_item
             ({ str_desc = Tstr_value (Nonrecursive, bound); _ }, _) )
       | ( _
         , Browse_raw.Expression
             { exp_desc = Texp_let (Nonrecursive, bound, _); _ } ) ->
         let bound_vars = let_bound_vars bound in
         if
           List.exists bound_vars ~f:(fun (id, _) ->
               String.equal ident (Ident.name id))
         then
           (* Return the location of the first pattern in the let binding (the
              rec goes right before it) *)
           let+ first_pat = List.hd_opt bound in
           let first_pat_loc = first_pat.vb_pat.pat_loc in
           { first_pat_loc with loc_end = first_pat_loc.loc_start }
         else
           None
       | _ -> None)

let code_action_add_rec uri diagnostics doc loc =
  let edit =
    let textedit : TextEdit.t =
      { range = Range.of_loc loc; newText = "rec " }
    in
    let version = Document.version doc in
    let textDocument =
      OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
    in
    let edit =
      TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ]
    in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  CodeAction.create ~diagnostics ~title:action_title
    ~kind:CodeActionKind.QuickFix ~edit ~isPreferred:false ()

let code_action doc (params : CodeActionParams.t) =
  let open Fiber.O in
  let pos_start = Position.logical params.range.start in

  let m_diagnostic =
    List.find params.context.diagnostics ~f:(fun d ->
        let is_unbound () =
          String.is_prefix d.Diagnostic.message ~prefix:"Unbound value"
        and in_range () =
          match Position.compare_inclusion params.range.start d.range with
          | `Outside _ -> false
          | `Inside -> true
        in
        in_range () && is_unbound ())
  in
  match m_diagnostic with
  | Some d ->
    let+ loc =
      Document.with_pipeline_exn doc (fun pipeline ->
          has_missing_rec pipeline pos_start)
    in
    Option.map loc ~f:(code_action_add_rec params.textDocument.uri [ d ] doc)
  | None -> Fiber.return None
