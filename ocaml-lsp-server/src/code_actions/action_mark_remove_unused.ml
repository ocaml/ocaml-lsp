open Import

let slice doc (range : Range.t) =
  let src = Document.source doc in
  let (`Offset start) = Msource.get_offset src @@ Position.logical range.start
  and (`Offset end_) = Msource.get_offset src @@ Position.logical range.end_ in
  String.sub (Msource.text src) ~pos:start ~len:(end_ - start)

(* Return contexts enclosing `pos` in order from most specific to most
   general. *)
let enclosing_pos pipeline pos =
  let browse =
    Mpipeline.typer_result pipeline
    |> Mtyper.get_typedtree |> Mbrowse.of_typedtree
  in
  Mbrowse.enclosing
    (Mpipeline.get_lexing_pos pipeline @@ Position.logical pos)
    [ browse ]

(* `name` is an unused binding. `contexts` is a list of Mbrowse.t enclosing an
   unused definition of `name`, in order from most general to most specific.

   Returns an edit that silences the 'unused value' warning. *)
let rec mark_value_unused_edit name contexts =
  let open Option.O in
  match contexts with
  | Browse_raw.Pattern { pat_desc = Tpat_record (pats, _); _ } :: cs -> (
    let m_field_edit =
      List.find_map pats
        ~f:
          (function
           | ( { loc = field_loc; _ }
             , _
             , { pat_desc = Tpat_var (ident, _); pat_loc; _ } )
             when Ident.name ident = name ->
             (* Special case for record shorthand *)
             if
               field_loc.loc_start = pat_loc.loc_start
               && field_loc.loc_end = pat_loc.loc_end
             then
               let+ end_pos = Position.of_lexical_position pat_loc.loc_end in
               TextEdit.
                 { range = Range.create ~start:end_pos ~end_:end_pos
                 ; newText = " = _"
                 }
             else
               let+ start_pos =
                 Position.of_lexical_position pat_loc.loc_start
               in
               TextEdit.
                 { range = Range.create ~start:start_pos ~end_:start_pos
                 ; newText = "_"
                 }
           | _ -> None
            :    Longident.t Loc.loc
                 * Types.label_description
                 * Typedtree.value Typedtree.general_pattern
              -> TextEdit.t option)
    in
    match m_field_edit with
    | Some e -> Some e
    | None -> mark_value_unused_edit name cs)
  | Pattern { pat_desc = Tpat_var (ident, _); pat_loc = loc; _ } :: _ ->
    if Ident.name ident = name then
      let+ start = Position.of_lexical_position loc.loc_start in
      TextEdit.{ range = Range.create ~start ~end_:start; newText = "_" }
    else
      None
  | _ :: cs -> mark_value_unused_edit name cs
  | _ -> None

let code_action_mark_value_unused doc (diagnostic : Diagnostic.t) =
  let open Option.O in
  Document.with_pipeline_exn doc (fun pipeline ->
      let var_name = slice doc diagnostic.range in
      let pos = diagnostic.range.start in
      let+ text_edit =
        enclosing_pos pipeline pos
        |> List.rev_map ~f:(fun (_, x) -> x)
        |> mark_value_unused_edit var_name
      in
      let edit = Document.edit doc text_edit in
      CodeAction.create ~diagnostics:[ diagnostic ] ~title:"Mark as unused"
        ~kind:CodeActionKind.QuickFix ~edit ~isPreferred:false ())

(* Takes a list of contexts enclosing a binding of `name`. Returns the range of
   the most specific binding. *)
let enclosing_value_binding_range name =
  let open Option.O in
  List.find_map ~f:(function
    | Browse_raw.Expression
        { exp_desc =
            Texp_let
              ( _
              , [ { vb_pat = { pat_desc = Tpat_var (_, { txt = name'; _ }); _ }
                  ; _
                  }
                ]
              , body )
        ; exp_loc
        ; _
        }
      when name = name' ->
      let* start = Position.of_lexical_position exp_loc.loc_start in
      let+ end_ = Position.of_lexical_position body.exp_loc.loc_start in
      Range.create ~start ~end_
    | _ -> None)

(* Create a code action that removes [range] and refers to [diagnostic]. *)
let code_action_remove_range doc (diagnostic : Diagnostic.t) range =
  let edit = Document.edit doc { range; newText = "" } in
  CodeAction.create ~diagnostics:[ diagnostic ] ~title:"Remove unused"
    ~kind:CodeActionKind.QuickFix ~edit ~isPreferred:false ()

(* Create a code action that removes the value mentioned in [diagnostic]. *)
let code_action_remove_value doc pos (diagnostic : Diagnostic.t) =
  Document.with_pipeline_exn doc (fun pipeline ->
      let var_name = slice doc diagnostic.range in
      enclosing_pos pipeline pos
      |> List.map ~f:(fun (_, x) -> x)
      |> enclosing_value_binding_range var_name
      |> Option.map ~f:(fun range ->
             code_action_remove_range doc diagnostic range))

let find_unused_diagnostic pos (ds : Diagnostic.t list) =
  List.find ds ~f:(fun d ->
      let in_range =
        match Position.compare_inclusion pos d.range with
        | `Outside _ -> false
        | `Inside -> true
      in
      in_range && Diagnostic_util.is_unused_var_warning d)

let code_action_mark doc (params : CodeActionParams.t) =
  let pos = params.range.start in
  match find_unused_diagnostic pos params.context.diagnostics with
  | None -> Fiber.return None
  | Some d -> code_action_mark_value_unused doc d

let code_action_remove doc (params : CodeActionParams.t) =
  let pos = params.range.start in
  match find_unused_diagnostic pos params.context.diagnostics with
  | None -> Fiber.return None
  | Some d -> code_action_remove_value doc pos d

let mark = { Code_action.kind = QuickFix; run = code_action_mark }

let remove = { Code_action.kind = QuickFix; run = code_action_remove }
