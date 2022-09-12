open Import

let action_title = "Inline"

type inline_task =
  { inlined_var : Ident.t
  ; inlined_expr : Typedtree.expression  (** the expression to inline *)
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
       | ( (_ : Ocaml_typing.Env.t)
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
         Some { inlined_var = id; inlined_expr = vb_expr; context = rhs }
       | _ -> None)

let find_parsetree_loc pipeline loc =
  let exception Found of Parsetree.expression in
  try
    let expr_iter (iter : Ast_iterator.iterator) (expr : Parsetree.expression) =
      if Loc.compare expr.pexp_loc loc = 0 then raise (Found expr)
      else Ast_iterator.default_iterator.expr iter expr
    in
    let iterator = { Ast_iterator.default_iterator with expr = expr_iter } in
    (match Mpipeline.reader_parsetree pipeline with
    | `Implementation s -> iterator.structure iterator s
    | `Interface _ -> ());
    None
  with Found e -> Some e

let inlined_text pipeline task =
  let open Option.O in
  let+ expr = find_parsetree_loc pipeline task.inlined_expr.exp_loc in
  Format.asprintf "(%a)" Ocaml_parsing.Pprintast.expression expr

(** Iterator over the text edits performed by the inlining task. *)
let inline_edits pipeline task =
  let open Option.O in
  let+ newText = inlined_text pipeline task in
  let make_edit newText loc =
    TextEdit.create ~newText ~range:(Range.of_loc loc)
  in
  let edits = Queue.create () in
  let insert_edit newText loc = Queue.push edits (make_edit newText loc) in

  let arg_iter (iter : Ocaml_typing.Tast_iterator.iterator)
      (label : Asttypes.arg_label) (m_arg_expr : Typedtree.expression option) =
    match (label, m_arg_expr) with
    (* handle the labeled argument shorthand `f ~x` when inlining `x` *)
    | ( Labelled name
      , Some { exp_desc = Texp_ident (Pident id, { loc; _ }, _); _ } )
    (* optional arguments have a different representation *)
    | ( Optional name
      , Some
          { exp_desc =
              Texp_construct
                ( _
                , _
                , [ { exp_desc = Texp_ident (Pident id, { loc; _ }, _); _ } ] )
          ; _
          } )
      when Ident.same task.inlined_var id ->
      let newText = sprintf "%s:%s" name newText in
      insert_edit newText loc
    | _, m_expr -> Option.iter m_expr ~f:(iter.expr iter)
  in

  let expr_iter (iter : Ocaml_typing.Tast_iterator.iterator)
      (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_apply (func, args) ->
      iter.expr iter func;
      List.iter args ~f:(fun (l, e) -> arg_iter iter l e)
    | Texp_ident (Pident id, { loc; _ }, _) when Ident.same task.inlined_var id
      -> insert_edit newText loc
    | _ -> Ocaml_typing.Tast_iterator.default_iterator.expr iter expr
  in
  let iterator =
    { Ocaml_typing.Tast_iterator.default_iterator with expr = expr_iter }
  in
  iterator.expr iterator task.context;
  Queue.to_list edits

let code_action doc (params : CodeActionParams.t) =
  let open Fiber.O in
  let* m_edits =
    Document.with_pipeline_exn doc (fun pipeline ->
        find_inline_task pipeline params.range.start
        |> Option.bind ~f:(inline_edits pipeline))
  in
  Option.map m_edits ~f:(fun edits ->
      let edit =
        let version = Document.version doc in
        let textDocument =
          OptionalVersionedTextDocumentIdentifier.create
            ~uri:params.textDocument.uri ~version ()
        in
        let edit =
          TextDocumentEdit.create ~textDocument
            ~edits:(List.map edits ~f:(fun e -> `TextEdit e))
        in
        WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
      in
      CodeAction.create ~title:action_title ~kind:CodeActionKind.RefactorInline
        ~edit ~isPreferred:false ())
  |> Fiber.return

module Test = struct
  let x =
    let k = 0 in
    let f ?(k = 1) ~j () = k + j in
    f ~j:0 ~k ()
end

let t = { Code_action.kind = RefactorInline; run = code_action }
