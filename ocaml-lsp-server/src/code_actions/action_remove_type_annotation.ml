open Import

let action_kind = "remove type annotation"

let check_typeable_context pipeline pos_start =
  let pos_start = Mpipeline.get_lexing_pos pipeline pos_start in
  let typer = Mpipeline.typer_result pipeline in
  let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
  let is_exp_constrained = function
    | Typedtree.Texp_constraint _, loc, _ -> Some loc
    | Typedtree.Texp_coerce (Some { ctyp_loc; _ }, _), _, _ -> Some ctyp_loc
    | _ -> None
  in
  let is_pat_constrained = function
    | Typedtree.Tpat_constraint _, loc, _ -> Some loc
    | _ -> None
  in
  let is_valid loc p extras =
    (* Constrains are listed from the farthest to the closest. We search
       reversed list to find the closest type annotation to remove. *)
    match extras |> List.rev |> List.find_map ~f:p with
    | Some x -> `Valid (loc, x)
    | None -> `Invalid
  in
  match Mbrowse.enclosing pos_start [ browse ] with
  | (_, Pattern { pat_desc = Tpat_var _; _ })
    :: (_, Value_binding { vb_expr = { exp_desc = Texp_function _; _ }; _ })
    :: _ -> `Invalid
  | (_, Expression e) :: _ -> is_valid e.exp_loc is_exp_constrained e.exp_extra
  | (_, Pattern { pat_desc = Typedtree.Tpat_any; pat_loc; _ })
    :: (_, Pattern { pat_desc = Typedtree.Tpat_alias _; pat_extra; _ })
    :: _ -> is_valid pat_loc is_pat_constrained pat_extra
  | (_, Pattern p) :: _ -> is_valid p.pat_loc is_pat_constrained p.pat_extra
  | _ :: _ | [] -> `Invalid
;;

let get_source_text doc (loc : Loc.t) =
  let open Option.O in
  let source = Document.source doc in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  let (`Offset start) = Msource.get_offset source (Position.logical start) in
  let (`Offset end_) = Msource.get_offset source (Position.logical end_) in
  String.sub (Msource.text source) ~pos:start ~len:(end_ - start)
;;

let code_action_of_type_enclosing uri doc (loc, constr_loc) =
  let open Option.O in
  let+ src_text = get_source_text doc loc in
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t =
      { range = Range.of_loc (Loc.union loc constr_loc); newText = src_text }
    in
    let version = Document.version doc in
    let textDocument = OptionalVersionedTextDocumentIdentifier.create ~uri ~version () in
    let edit = TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ] in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  let title = String.capitalize_ascii action_kind in
  CodeAction.create
    ~title
    ~kind:(CodeActionKind.Other action_kind)
    ~edit
    ~isPreferred:false
    ()
;;

let code_action pipeline doc (params : CodeActionParams.t) =
  let pos_start = Position.logical params.range.start in
  let context = check_typeable_context pipeline pos_start in
  match context with
  | `Invalid -> None
  | `Valid (loc1, loc2) ->
    code_action_of_type_enclosing params.textDocument.uri doc (loc1, loc2)
;;

let kind = CodeActionKind.Other action_kind
let t = Code_action.batchable kind code_action
