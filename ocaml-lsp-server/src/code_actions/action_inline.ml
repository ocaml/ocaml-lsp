open Import

let action_title = "Inline"

type inline_task =
  { inlined_var : Ident.t
  ; inlined_expr : Typedtree.expression  (** the expression to inline *)
  ; context : Typedtree.expression  (** where to perform inlining *)
  }

let find_path_by_name id env =
  try Some (fst (Ocaml_typing.Env.find_value_by_name id env))
  with Not_found -> None

let check_shadowing (inlined_expr : Typedtree.expression) new_env =
  let module I = Ocaml_typing.Tast_iterator in
  let orig_env = inlined_expr.exp_env in
  let exception Env_mismatch of (Longident.t * [ `Unbound | `Shadowed ]) in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (path, { txt = ident; _ }, _) -> (
      let in_orig_env =
        find_path_by_name ident orig_env
        |> Option.map ~f:(Path.same path)
        |> Option.value ~default:false
      in
      if in_orig_env then
        match find_path_by_name ident new_env with
        | Some path' ->
          if not (Path.same path path') then
            raise (Env_mismatch (ident, `Shadowed))
        | None -> raise (Env_mismatch (ident, `Unbound)))
    | _ -> I.default_iterator.expr iter expr
  in
  let iter = { I.default_iterator with expr = expr_iter } in
  try
    iter.expr iter inlined_expr;
    Ok ()
  with Env_mismatch m -> Error m

let string_of_error (ident, reason) =
  let reason =
    match reason with
    | `Unbound -> "unbound"
    | `Shadowed -> "shadowed"
  in
  Format.asprintf "'%a' is %s in inlining context" Pprintast.longident ident
    reason

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

(** [strip_attribute name e] removes all instances of the attribute called
    [name] in [e]. *)
let strip_attribute attr_name expr =
  let module M = Ocaml_parsing.Ast_mapper in
  let expr_map (map : M.mapper) expr =
    { (M.default_mapper.expr map expr) with
      pexp_attributes =
        List.filter expr.pexp_attributes ~f:(fun (a : Parsetree.attribute) ->
            not (String.equal a.attr_name.txt attr_name))
    }
  in
  let mapper = { M.default_mapper with expr = expr_map } in
  mapper.expr mapper expr

let inlined_text pipeline task =
  let open Option.O in
  let+ expr = find_parsetree_loc pipeline task.inlined_expr.exp_loc in
  let expr = strip_attribute "merlin.loc" expr in
  Format.asprintf "(%a)" Pprintast.expression expr

(** Iterator over the text edits performed by the inlining task. *)
let inline_edits pipeline task =
  let module I = Ocaml_typing.Tast_iterator in
  let open Option.O in
  let+ newText = inlined_text pipeline task in
  let make_edit newText loc =
    TextEdit.create ~newText ~range:(Range.of_loc loc)
  in
  let edits = Queue.create () in
  let error = ref None in

  let insert_edit newText loc = Queue.push edits (make_edit newText loc) in
  let not_shadowed env =
    match check_shadowing task.inlined_expr env with
    | Ok () -> true
    | Error e ->
      error := Some e;
      false
  in

  let arg_iter env (iter : I.iterator) (label : Asttypes.arg_label)
      (m_arg_expr : Typedtree.expression option) =
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
      when Ident.same task.inlined_var id && not_shadowed env ->
      let newText = sprintf "%s:%s" name newText in
      insert_edit newText loc
    | _, m_expr -> Option.iter m_expr ~f:(iter.expr iter)
  in

  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_apply (func, args) ->
      iter.expr iter func;
      List.iter args ~f:(fun (l, e) -> arg_iter expr.exp_env iter l e)
    | Texp_ident (Pident id, { loc; _ }, _)
      when Ident.same task.inlined_var id && not_shadowed expr.exp_env ->
      insert_edit newText loc
    | _ -> I.default_iterator.expr iter expr
  in
  let iterator = { I.default_iterator with expr = expr_iter } in
  iterator.expr iterator task.context;
  (Queue.to_list edits, !error)

let code_action doc (params : CodeActionParams.t) =
  let open Fiber.O in
  let* m_edits =
    Document.with_pipeline_exn doc (fun pipeline ->
        find_inline_task pipeline params.range.start
        |> Option.bind ~f:(inline_edits pipeline))
  in
  Option.bind m_edits ~f:(fun (edits, m_error) ->
      match (edits, m_error) with
      | [], None -> None
      | [], Some error ->
        let action =
          CodeAction.create ~title:action_title
            ~kind:CodeActionKind.RefactorInline ~isPreferred:false
            ~disabled:
              (CodeAction.create_disabled ~reason:(string_of_error error))
            ()
        in
        Some action
      | _ :: _, (Some _ | None) ->
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
        let action =
          CodeAction.create ~title:action_title
            ~kind:CodeActionKind.RefactorInline ~edit ~isPreferred:false ()
        in
        Some action)
  |> Fiber.return

module Test = struct
  let x =
    let k = 0 in
    let f ?(k = 1) ~j () = k + j in
    f ~j:0 ~k ()
end

let t = { Code_action.kind = RefactorInline; run = code_action }
