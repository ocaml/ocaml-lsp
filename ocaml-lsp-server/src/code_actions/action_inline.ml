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
  Format.asprintf
    "'%a' is %s in inlining context"
    Pprintast.longident
    ident
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

(** [uses expr path] returns the number of uses of [path] in [expr]. *)
let uses expr path =
  let module I = Ocaml_typing.Tast_iterator in
  let count = ref 0 in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (path', _, _) when Path.same path path' -> incr count
    | _ -> I.default_iterator.expr iter expr
  in
  let iterator = { I.default_iterator with expr = expr_iter } in
  iterator.expr iterator expr;
  !count

let subst task =
  let module M = Ocaml_typing.Tast_mapper in
  let expr_map (map : M.mapper) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (Pident id, _, _) when Ident.same task.inlined_var id ->
      task.inlined_expr
    | _ -> M.default.expr map expr
  in
  let mapper = { M.default with expr = expr_map } in
  mapper.expr mapper task.context

(** Rough check for pure expressions. Identifiers and constants are pure. *)
let is_pure (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident _ | Texp_constant _ -> true
  | _ -> false

let rec beta_reduce (app : Typedtree.expression) =
  let untype_expression = Ocaml_typing.Untypeast.untype_expression in
  match app.exp_desc with
  | Texp_apply
      ( { exp_desc =
            Texp_function
              { arg_label = Nolabel
              ; cases =
                  [ { c_lhs = { pat_desc = pat; _ }
                    ; c_guard = None
                    ; c_rhs = body
                    }
                  ]
              ; _
              }
        ; _
        }
      , (Nolabel, Some arg) :: args' ) -> (
    let body =
      if List.is_empty args' then body
      else { app with exp_desc = Texp_apply (body, args') }
    in
    match pat with
    | Tpat_any -> beta_reduce body
    | Tpat_var (param, _) ->
      let n_uses = uses body (Path.Pident param) in
      Printf.eprintf "uses: %d\n%!" n_uses;
      Out_channel.flush stderr;
      if n_uses = 0 then Ocaml_typing.Untypeast.untype_expression body
      else if n_uses = 1 || is_pure arg then
        beta_reduce
          (subst { inlined_var = param; inlined_expr = arg; context = body })
      else
        (* if the parameter is used multiple times in the body, introduce a let
           binding so that the parameter is evaluated only once *)
        let module H = Ocaml_parsing.Ast_helper in
        let body = untype_expression body in
        let arg = untype_expression arg in
        H.Exp.let_
          Nonrecursive
          [ H.Vb.mk
              (H.Pat.var { txt = Ident.name param; loc = !H.default_loc })
              arg
          ]
          body
    | _ -> untype_expression app)
  | _ -> untype_expression app

module Test = struct
  let z =
    let k = 1 in
    let f y = y + k in
    let k = 2 in
    let y = 2 in
    f y
end

let inlined_text pipeline task =
  let open Option.O in
  let+ expr = find_parsetree_loc pipeline task.inlined_expr.exp_loc in
  let expr = strip_attribute "merlin.loc" expr in
  Format.asprintf "(%a)" Pprintast.expression expr

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

  (* inlining into an argument context has some special cases *)
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

  let apply_iter env (iter : I.iterator) (func : Typedtree.expression) args =
    iter.expr iter func;
    List.iter args ~f:(fun (l, e) -> arg_iter env iter l e)
  in

  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    (* when inlining into an application context, attempt to beta reduce the
       result *)
    | Texp_apply ({ exp_desc = Texp_ident (Pident id, _, _); _ }, args)
      when Ident.same task.inlined_var id && not_shadowed expr.exp_env ->
      let reduced_expr =
        beta_reduce
          { expr with exp_desc = Texp_apply (task.inlined_expr, args) }
      in
      let newText =
        Format.asprintf "(%a)" Pprintast.expression
        @@ strip_attribute "merlin.loc" reduced_expr
      in
      insert_edit newText expr.exp_loc
    | Texp_apply (func, args) -> apply_iter expr.exp_env iter func args
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
          CodeAction.create
            ~title:action_title
            ~kind:CodeActionKind.RefactorInline
            ~isPreferred:false
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
              ~uri:params.textDocument.uri
              ~version
              ()
          in
          let edit =
            TextDocumentEdit.create
              ~textDocument
              ~edits:(List.map edits ~f:(fun e -> `TextEdit e))
          in
          WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
        in
        let action =
          CodeAction.create
            ~title:action_title
            ~kind:CodeActionKind.RefactorInline
            ~edit
            ~isPreferred:false
            ()
        in
        Some action)
  |> Fiber.return

let t = { Code_action.kind = RefactorInline; run = code_action }
