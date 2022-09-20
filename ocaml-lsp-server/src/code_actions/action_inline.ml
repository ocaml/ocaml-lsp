open Import
module H = Ocaml_parsing.Ast_helper

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

let rec find_map_until ~f = function
  | [] -> None
  | x :: xs -> (
    match f x with
    | `Return x' -> Some x'
    | `Skip -> find_map_until ~f xs
    | `Done -> None)

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
  |> find_map_until ~f:(fun (_, expr) ->
         if contains (Mbrowse.node_loc expr) pos then
           match expr with
           | Browse_raw.Expression
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
               }
             when contains s.loc pos ->
             `Return { inlined_var = id; inlined_expr = vb_expr; context = rhs }
           | _ -> `Skip
         else `Done)

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

let find_parsetree_loc_exn pipeline loc =
  Option.value_exn (find_parsetree_loc pipeline loc)

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

module Uses = struct
  type t = int Path.Map.t

  let find m k = Path.Map.find_opt k m

  let of_typedtree (expr : Typedtree.expression) =
    let module I = Ocaml_typing.Tast_iterator in
    let uses = ref Path.Map.empty in
    let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
      match expr.exp_desc with
      | Texp_ident (path, _, _) ->
        uses :=
          Path.Map.update
            path
            (function
              | Some c -> Some (c + 1)
              | None -> Some 1)
            !uses
      | _ -> I.default_iterator.expr iter expr
    in
    let iterator = { I.default_iterator with expr = expr_iter } in
    iterator.expr iterator expr;
    !uses
end

module Paths = struct
  module Location_map = Map.Make (struct
    include Loc

    let compare x x' = Ordering.of_int (compare x x')

    let position_to_dyn (pos : Lexing.position) =
      Dyn.Record
        [ ("pos_fname", Dyn.String pos.pos_fname)
        ; ("pos_lnum", Dyn.Int pos.pos_lnum)
        ; ("pos_bol", Dyn.Int pos.pos_bol)
        ; ("pos_cnum", Dyn.Int pos.pos_cnum)
        ]

    let to_dyn loc =
      Dyn.Record
        [ ("loc_start", position_to_dyn loc.loc_start)
        ; ("loc_end", position_to_dyn loc.loc_end)
        ; ("loc_ghost", Dyn.Bool loc.loc_ghost)
        ]
  end)

  type t = Path.t Location_map.t

  let find = Location_map.find

  let of_typedtree (expr : Typedtree.expression) =
    let module I = Ocaml_typing.Tast_iterator in
    let paths = ref Location_map.empty in
    let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
      match expr.exp_desc with
      | Texp_ident (path, { loc; _ }, _) ->
        paths := Location_map.set !paths loc path
      | _ -> I.default_iterator.expr iter expr
    in
    let pat_iter (type k) (iter : I.iterator)
        (pat : k Typedtree.general_pattern) =
      match pat.pat_desc with
      | Tpat_var (id, { loc; _ }) ->
        paths := Location_map.set !paths loc (Pident id)
      | _ -> I.default_iterator.pat iter pat
    in
    let iterator =
      { I.default_iterator with expr = expr_iter; pat = pat_iter }
    in
    iterator.expr iterator expr;
    !paths

  let same_path ps l l' =
    match (find ps l, find ps l') with
    | Some p, Some p' -> Path.same p p'
    | _ -> false
end

let subst same subst_expr subst_id body =
  let module M = Ocaml_parsing.Ast_mapper in
  let expr_map (map : M.mapper) (expr : Parsetree.expression) =
    match expr.pexp_desc with
    | Pexp_ident id when same subst_id id -> subst_expr
    | _ -> M.default_mapper.expr map expr
  in
  let mapper = { M.default_mapper with expr = expr_map } in
  mapper.expr mapper body

(** Rough check for expressions that can be duplicated without duplicating any
    side effects. *)
let rec is_pure (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_ident _ | Pexp_constant _ -> true
  | Pexp_field (lhs, _) -> is_pure lhs
  | _ -> false

let rec beta_reduce (uses : Uses.t) (paths : Paths.t)
    (app : Parsetree.expression) =
  match app.pexp_desc with
  | Pexp_apply
      ( { pexp_desc = Pexp_fun (Nolabel, None, pat, body); _ }
      , (Nolabel, arg) :: args' ) -> (
    let body = if List.is_empty args' then body else H.Exp.apply body args' in
    match pat.ppat_desc with
    | Ppat_any | Ppat_construct ({ txt = Lident "()"; _ }, _) ->
      beta_reduce uses paths body
    | Ppat_var param -> (
      let open Option.O in
      let m_uses =
        let* path = Paths.find paths param.loc in
        Uses.find uses path
      in
      let same_path paths (id : _ H.with_loc) (id' : _ H.with_loc) =
        Paths.same_path paths id.loc id'.loc
      in
      match m_uses with
      | Some 0 -> beta_reduce uses paths body
      | Some 1 ->
        beta_reduce uses paths (subst (same_path paths) arg param body)
      | Some _ | None ->
        if is_pure arg then
          beta_reduce uses paths (subst (same_path paths) arg param body)
        else
          (* if the parameter is used multiple times in the body, introduce a
             let binding so that the parameter is evaluated only once *)
          H.Exp.let_
            Nonrecursive
            [ H.Vb.mk pat arg ]
            (beta_reduce uses paths body))
    | _ ->
      H.Exp.let_ Nonrecursive [ H.Vb.mk pat arg ] (beta_reduce uses paths body))
  | _ -> app

module Test = struct
  type t = { x : int }

  let z =
    let f y = y + 1 in
    let y = { x = 0 } in
    f y.x
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

  let uses = Uses.of_typedtree task.inlined_expr in
  let paths = Paths.of_typedtree task.inlined_expr in
  let inlined_pexpr =
    find_parsetree_loc_exn pipeline task.inlined_expr.exp_loc
  in

  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    (* when inlining into an application context, attempt to beta reduce the
       result *)
    | Texp_apply ({ exp_desc = Texp_ident (Pident id, _, _); _ }, _)
      when Ident.same task.inlined_var id && not_shadowed expr.exp_env ->
      let reduced_pexpr =
        let app_pexpr = find_parsetree_loc_exn pipeline expr.exp_loc in
        match app_pexpr.pexp_desc with
        | Pexp_apply ({ pexp_desc = Pexp_ident _; _ }, args) ->
          beta_reduce uses paths (H.Exp.apply inlined_pexpr args)
        | _ -> app_pexpr
      in
      let newText =
        Format.asprintf "(%a)" Pprintast.expression
        @@ strip_attribute "merlin.loc" reduced_pexpr
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
