open Import

let action_title = "Inline"

type inline_task =
  { ident : Ident.t
  ; body : Typedtree.expression  (** the expression to inline *)
  ; context : Typedtree.expression  (** where to perform inlining *)
  }

let find_inline_task pipeline pos =
  let contains loc pos =
    match Position.compare_inclusion pos (Range.of_loc loc) with
    | `Outside _ -> false
    | `Inside -> true
  in

  (* Find most enclosing nonrecursive let binding *)
  let browse =
    Mpipeline.typer_result pipeline
    |> Mtyper.get_typedtree |> Mbrowse.of_typedtree
  in

  Mbrowse.enclosing
    (Mpipeline.get_lexing_pos pipeline (Position.logical pos))
    [ browse ]
  |> List.find_map ~f:(function
       | ( _
         , Browse_raw.Expression
             { exp_desc =
                 Texp_let
                   ( Nonrecursive
                   , [ { vb_pat = { pat_desc = Tpat_var (id, s); _ }
                       ; vb_expr
                       ; _
                       }
                     ]
                   , rhs )
             ; _
             } )
         when contains s.loc pos ->
         Some { ident = id; body = vb_expr; context = rhs }
       | _ -> None)

let find_parsetree_loc pipeline loc k =
  let expr_iter (iter : Ocaml_parsing.Ast_iterator.iterator)
      (expr : Parsetree.expression) =
    if expr.pexp_loc = loc then k expr
    else Ocaml_parsing.Ast_iterator.default_iterator.expr iter expr
  in
  let iterator =
    { Ocaml_parsing.Ast_iterator.default_iterator with expr = expr_iter }
  in
  match Mpipeline.reader_parsetree pipeline with
  | `Implementation s -> iterator.structure iterator s
  | _ -> ()

let inlined_text pipeline task =
  let ret = ref None in
  find_parsetree_loc pipeline task.body.exp_loc (fun expr ->
      ret :=
        Some (Format.asprintf "(%a)" Ocaml_parsing.Pprintast.expression expr));
  Option.value_exn !ret

(** Iterator over the text edits performed by the inlining task. *)
let iter_inline_edits pipeline task k =
  let newText = inlined_text pipeline task in
  let make_edit newText loc =
    TextEdit.create ~newText ~range:(Range.of_loc loc)
  in

  let expr_iter (iter : Ocaml_typing.Tast_iterator.iterator)
      (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_apply (func, args) ->
      iter.expr iter func;
      List.iter args
        ~f:(fun (label, (m_arg_expr : Typedtree.expression option)) ->
          match (label, m_arg_expr) with
          (* handle the labeled argument shorthand `f ~x` when inlining `x` *)
          | ( Asttypes.Labelled name
            , Some { exp_desc = Texp_ident (Pident id, { loc; _ }, _); _ } )
            when Ident.same task.ident id ->
            let newText = sprintf "%s:%s" name newText in
            k (make_edit newText loc)
          | _, m_expr -> Option.iter m_expr ~f:(iter.expr iter))
    | Texp_ident (Pident id, { loc; _ }, _) when Ident.same task.ident id ->
      k (make_edit newText loc)
    | _ -> Ocaml_typing.Tast_iterator.default_iterator.expr iter expr
  in
  let iterator =
    { Ocaml_typing.Tast_iterator.default_iterator with expr = expr_iter }
  in
  iterator.expr iterator task.context

let code_action doc (params : CodeActionParams.t) =
  Document.with_pipeline_exn doc (fun pipeline ->
      let m_inline_task = find_inline_task pipeline params.range.start in
      Option.map m_inline_task ~f:(fun task ->
          let edits = Queue.create () in
          iter_inline_edits pipeline task (Queue.push edits);

          let edit =
            let version = Document.version doc in
            let textDocument =
              OptionalVersionedTextDocumentIdentifier.create
                ~uri:params.textDocument.uri ~version ()
            in
            let edit =
              TextDocumentEdit.create ~textDocument
                ~edits:
                  (Queue.to_list edits |> List.map ~f:(fun e -> `TextEdit e))
            in
            WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
          in
          CodeAction.create ~title:action_title
            ~kind:CodeActionKind.RefactorInline ~edit ~isPreferred:false ()))

let t = { Code_action.kind = RefactorInline; run = code_action }
