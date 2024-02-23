open Import
open Option.O

let diagnostic_regex, diagnostic_regex_marks =
  let msgs =
    ( Re.mark
        (Re.alt
           [ Re.str "Error (warning 26)"
           ; Re.str "Error (warning 27)"
           ; Re.str "unused value"
           ])
    , `Value )
    :: ([ "unused open", `Open
        ; "unused open!", `Open_bang
        ; "unused type", `Type
        ; "unused constructor", `Constructor
        ; "unused extension constructor", `Extension
        ; "this match case is unused", `Case
        ; "unused for-loop index", `For_loop_index
        ; "unused rec flag", `Rec
        ; "unused module", `Module
        ]
        |> List.map ~f:(fun (msg, kind) -> Re.mark (Re.str msg), kind))
  in
  let regex =
    Re.compile (Re.seq [ Re.bol; Re.alt (List.map ~f:(fun ((_, r), _) -> r) msgs) ])
  in
  let marks = List.map ~f:(fun ((m, _), k) -> m, k) msgs in
  regex, marks
;;

let find_unused_diagnostic pos ds =
  let open Option.O in
  List.filter ds ~f:(fun (d : Diagnostic.t) ->
    match Position.compare_inclusion pos d.range with
    | `Outside _ -> false
    | `Inside -> true)
  |> List.find_map ~f:(fun (d : Diagnostic.t) ->
    let* group =
      let message =
        match d.message with
        | `String m -> m
        | `MarkupContent { value = m; _ } ->
          (* TODO: this is wrong *)
          m
      in
      Re.exec_opt diagnostic_regex message
    in
    let+ kind =
      List.find_map diagnostic_regex_marks ~f:(fun (m, k) ->
        if Re.Mark.test group m then Some k else None)
    in
    kind, d)
;;

(* Return contexts enclosing `pos` in order from most specific to most
   general. *)
let enclosing_pos pipeline pos =
  let browse =
    Mpipeline.typer_result pipeline |> Mtyper.get_typedtree |> Mbrowse.of_typedtree
  in
  Mbrowse.enclosing (Mpipeline.get_lexing_pos pipeline @@ Position.logical pos) [ browse ]
;;

(* `name` is an unused binding. `contexts` is a list of Mbrowse.t enclosing an
   unused definition of `name`, in order from most general to most specific.
   Returns an edit that silences the 'unused value' warning. *)
let rec mark_value_unused_edit name contexts =
  match contexts with
  | Browse_raw.Pattern { pat_desc = Tpat_record (pats, _); _ } :: cs ->
    let m_field_edit =
      List.find_map
        pats
        ~f:
          (function
           | { loc = field_loc; _ }, _, { pat_desc = Tpat_var (ident, _, _); pat_loc; _ }
             when Ident.name ident = name ->
             (* Special case for record shorthand *)
             if field_loc.loc_start = pat_loc.loc_start
                && field_loc.loc_end = pat_loc.loc_end
             then
               let+ end_pos = Position.of_lexical_position pat_loc.loc_end in
               TextEdit.
                 { range = Range.create ~start:end_pos ~end_:end_pos; newText = " = _" }
             else
               let+ start_pos = Position.of_lexical_position pat_loc.loc_start in
               TextEdit.
                 { range = Range.create ~start:start_pos ~end_:start_pos; newText = "_" }
           | _ -> None
           : Longident.t Loc.loc
             * Types.label_description
             * Typedtree.value Typedtree.general_pattern
             -> TextEdit.t option)
    in
    (match m_field_edit with
     | Some e -> Some e
     | None -> mark_value_unused_edit name cs)
  | Pattern { pat_desc = Tpat_var (ident, _, _); pat_loc = loc; _ } :: _ ->
    if Ident.name ident = name
    then
      let+ start = Position.of_lexical_position loc.loc_start in
      { TextEdit.range = Range.create ~start ~end_:start; newText = "_" }
    else None
  | _ :: cs -> mark_value_unused_edit name cs
  | _ -> None
;;

let code_action_mark_value_unused pipeline doc (diagnostic : Diagnostic.t) =
  let open Option.O in
  let* var_name = Document.substring doc diagnostic.range in
  let pos = diagnostic.range.start in
  let+ text_edit =
    enclosing_pos pipeline pos |> List.rev_map ~f:snd |> mark_value_unused_edit var_name
  in
  let edit = Document.edit doc [ text_edit ] in
  CodeAction.create
    ~diagnostics:[ diagnostic ]
    ~title:"Mark as unused"
    ~kind:CodeActionKind.QuickFix
    ~edit
    ~isPreferred:true
    ()
;;

(* Takes a list of contexts enclosing a binding of `name`. Returns the range of
   the most specific binding. *)
let enclosing_value_binding_range name =
  List.find_map ~f:(function
    | Browse_raw.Expression
        { exp_desc =
            Texp_let
              ( _
              , [ { vb_pat = { pat_desc = Tpat_var (_, { txt = name'; _ }, _); _ }; _ } ]
              , { exp_loc = { loc_start = let_end; _ }; _ } )
        ; exp_loc = { loc_start = let_start; _ }
        ; _
        }
      when name = name' ->
      let* start = Position.of_lexical_position let_start in
      let+ end_ = Position.of_lexical_position let_end in
      Range.create ~start ~end_
    | _ -> None)
;;

(* Create a code action that removes [range] and refers to [diagnostic]. *)
let code_action_remove_range
  ?(title = "Remove unused")
  doc
  (diagnostic : Diagnostic.t)
  range
  =
  let edit = Document.edit doc [ { range; newText = "" } ] in
  CodeAction.create
    ~diagnostics:[ diagnostic ]
    ~title
    ~kind:CodeActionKind.QuickFix
    ~edit
    ~isPreferred:false
    ()
;;

(* Create a code action that removes the value mentioned in [diagnostic]. *)
let code_action_remove_value pipeline doc pos (diagnostic : Diagnostic.t) =
  let* var_name = Document.substring doc diagnostic.range in
  enclosing_pos pipeline pos
  |> List.map ~f:snd
  |> enclosing_value_binding_range var_name
  |> Option.map ~f:(fun range -> code_action_remove_range doc diagnostic range)
;;

(** [create_mark_action ~title doc pos d] creates a code action that resolves
    the diagnostic [d] by inserting an underscore at [pos] in [doc]. *)
let create_mark_action ~title doc pos d =
  let edit =
    Document.edit doc [ { range = Range.create ~start:pos ~end_:pos; newText = "_" } ]
  in
  CodeAction.create
    ~diagnostics:[ d ]
    ~title
    ~kind:CodeActionKind.QuickFix
    ~edit
    ~isPreferred:true
    ()
;;

let action_mark_type pipeline doc pos (d : Diagnostic.t) =
  let open Option.O in
  let m_name_loc_start =
    enclosing_pos pipeline pos
    |> List.find_map ~f:(fun (_, node) ->
      match node with
      | Browse_raw.Type_declaration { typ_name = { loc = { loc_start; _ }; _ }; _ } ->
        Some loc_start
      | _ -> None)
  in
  let* name_loc_start = m_name_loc_start in
  let+ start = Position.of_lexical_position name_loc_start in
  create_mark_action ~title:"Mark type as unused" doc start d
;;

let contains loc pos =
  match Position.compare_inclusion pos (Range.of_loc loc) with
  | `Outside _ -> false
  | `Inside -> true
;;

let action_mark_for_loop_index pipeline doc pos (d : Diagnostic.t) =
  let open Option.O in
  let module I = Ocaml_parsing.Ast_iterator in
  let exception Found of Warnings.loc in
  let iterator =
    let expr iter (e : Parsetree.expression) =
      if contains e.pexp_loc pos
      then (
        match e.pexp_desc with
        | Pexp_for ({ ppat_loc; _ }, _, _, _, _) when contains ppat_loc pos ->
          raise_notrace (Found ppat_loc)
        | _ -> I.default_iterator.expr iter e)
    in
    let structure_item iter (item : Parsetree.structure_item) =
      if contains item.pstr_loc pos then I.default_iterator.structure_item iter item
    in
    { I.default_iterator with expr; structure_item }
  in
  let m_index_loc =
    match Mpipeline.reader_parsetree pipeline with
    | `Implementation parsetree ->
      (try
         iterator.structure iterator parsetree;
         None
       with
       | Found task -> Some task)
    | `Interface _ -> None
  in
  let* (index_loc : Warnings.loc) = m_index_loc in
  let+ start = Position.of_lexical_position index_loc.loc_start in
  create_mark_action ~title:"Mark for-loop index as unused" doc start d
;;

let action_mark_open doc (d : Diagnostic.t) =
  let edit =
    let pos = { d.range.start with character = d.range.start.character + 4 } in
    let range = Range.create ~start:pos ~end_:pos in
    Document.edit doc [ { range; newText = "!" } ]
  in
  CodeAction.create
    ~diagnostics:[ d ]
    ~title:"Replace with open!"
    ~kind:CodeActionKind.QuickFix
    ~edit
    ~isPreferred:true
    ()
;;

let rec_regex =
  Re.compile
    (Re.seq [ Re.bos; Re.rep Re.any; Re.group (Re.str "rec"); Re.rep Re.space; Re.stop ])
;;

let find_preceding doc pos regex =
  let open Option.O in
  let src = Document.source doc in
  let (`Offset end_) = Msource.get_offset src @@ Position.logical pos in
  let* groups = Re.exec_opt ~len:end_ regex (Msource.text src) in
  let match_start, match_end = Re.Group.offset groups 1 in
  let filename = Uri.to_path (Document.uri doc) in
  let* start =
    Msource.get_lexing_pos ~filename src (`Offset match_start)
    |> Position.of_lexical_position
  in
  let+ end_ =
    Msource.get_lexing_pos ~filename src (`Offset match_end)
    |> Position.of_lexical_position
  in
  Range.create ~start ~end_
;;

let action_remove_rec doc (d : Diagnostic.t) =
  let open Option.O in
  let+ rec_range = find_preceding doc d.range.start rec_regex in
  code_action_remove_range ~title:"Remove unused rec" doc d rec_range
;;

let bar_regex =
  Re.compile
    (Re.seq [ Re.bos; Re.rep Re.any; Re.group (Re.str "|"); Re.rep Re.space; Re.stop ])
;;

let action_remove_case pipeline doc (d : Diagnostic.t) =
  let open Option.O in
  let case_range =
    enclosing_pos pipeline d.range.start
    |> List.find_map ~f:(fun (_, node) ->
      match node with
      | Browse_raw.Case
          { c_lhs = { pat_loc = { loc_start; _ }; _ }
          ; c_rhs = { exp_loc = { loc_end; _ }; _ }
          ; _
          } -> Some (loc_start, loc_end)
      | _ -> None)
  in
  let* case_start, case_end = case_range in
  let* start = Position.of_lexical_position case_start in
  let* end_ = Position.of_lexical_position case_end in
  let+ preceding_bar = find_preceding doc start bar_regex in
  let edit =
    Document.edit
      doc
      [ { range = Range.create ~start:preceding_bar.start ~end_; newText = "" } ]
  in
  CodeAction.create
    ~diagnostics:[ d ]
    ~title:"Remove unused case"
    ~kind:CodeActionKind.QuickFix
    ~edit
    ~isPreferred:true
    ()
;;

let action_remove_constructor pipeline doc (d : Diagnostic.t) =
  let open Option.O in
  let case_range =
    enclosing_pos pipeline d.range.start
    |> List.find_map ~f:(fun (_, node) ->
      match node with
      | Browse_raw.Constructor_declaration { cd_loc = { loc_start; loc_end; _ }; _ } ->
        Some (loc_start, loc_end)
      | _ -> None)
  in
  let* case_start, case_end = case_range in
  let* start = Position.of_lexical_position case_start in
  let+ end_ = Position.of_lexical_position case_end in
  let edit = Document.edit doc [ { range = Range.create ~start ~end_; newText = "" } ] in
  CodeAction.create
    ~diagnostics:[ d ]
    ~title:"Remove unused constructor"
    ~kind:CodeActionKind.QuickFix
    ~edit
    ~isPreferred:true
    ()
;;

let action_remove_simple kind doc (d : Diagnostic.t) =
  code_action_remove_range ~title:("Remove unused " ^ kind) doc d d.range
;;

let mark =
  let run pipeline doc (params : CodeActionParams.t) =
    let open Option.O in
    let pos = params.range.start in
    let* diagnostic = find_unused_diagnostic pos params.context.diagnostics in
    match diagnostic with
    | `Value, d -> code_action_mark_value_unused pipeline doc d
    | `Open, d -> Some (action_mark_open doc d)
    | `Type, d -> action_mark_type pipeline doc pos d
    | `For_loop_index, d -> action_mark_for_loop_index pipeline doc pos d
    | (`Open_bang | `Constructor | `Extension | `Case | `Rec | `Module), _ ->
      (* these diagnostics don't have a reasonable "mark as unused" action *)
      None
  in
  Code_action.batchable QuickFix run
;;

let remove =
  let run pipeline doc (params : CodeActionParams.t) =
    let open Option.O in
    let pos = params.range.start in
    let* diagnostic = find_unused_diagnostic pos params.context.diagnostics in
    match diagnostic with
    | `Value, d -> code_action_remove_value pipeline doc pos d
    | `Open, d -> Some (action_remove_simple "open" doc d)
    | `Open_bang, d -> Some (action_remove_simple "open!" doc d)
    | `Type, d -> Some (action_remove_simple "type" doc d)
    | `Module, d -> Some (action_remove_simple "module" doc d)
    | `Case, d -> action_remove_case pipeline doc d
    | `Rec, d -> action_remove_rec doc d
    | `Constructor, d -> action_remove_constructor pipeline doc d
    | `Extension, _ ->
      (* todo *)
      None
    | `For_loop_index, _ ->
      (* these diagnostics don't have a reasonable "remove unused" action *)
      None
  in
  Code_action.batchable QuickFix run
;;
