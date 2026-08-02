open Import
open Fiber.O
module H = Ocaml_parsing.Ast_helper

let action_title = "Inline into uses"

type inline_task =
  { inlined_var : Ident.t
  ; inlined_expr : Typedtree.expression (** the expression to inline *)
  }

let find_path_by_name id env =
  try Some (fst (Ocaml_typing.Env.find_value_by_name id env)) with
  | Not_found -> None
;;

let check_shadowing (inlined_expr : Typedtree.expression) new_env =
  let module I = Ocaml_typing.Tast_iterator in
  let orig_env = inlined_expr.exp_env in
  let exception Env_mismatch of (Longident.t * [ `Unbound | `Shadowed ]) in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (path, { txt = ident; _ }, _) ->
      let in_orig_env =
        find_path_by_name ident orig_env
        |> Option.map ~f:(Path.same path)
        |> Option.value ~default:false
      in
      if in_orig_env
      then (
        match find_path_by_name ident new_env with
        | Some path' ->
          if not (Path.same path path')
          then raise_notrace (Env_mismatch (ident, `Shadowed))
        | None -> raise_notrace (Env_mismatch (ident, `Unbound)))
    | _ -> I.default_iterator.expr iter expr
  in
  let iter = { I.default_iterator with expr = expr_iter } in
  try
    iter.expr iter inlined_expr;
    Ok ()
  with
  | Env_mismatch m -> Error m
;;

let string_of_error (ident, reason) =
  let reason =
    match reason with
    | `Unbound -> "unbound"
    | `Shadowed -> "shadowed"
  in
  Format.asprintf "'%a' is %s in inlining context" Pprintast.longident ident reason
;;

let find_inline_task typedtree pos =
  let exception Found of inline_task in
  let module I = Ocaml_typing.Tast_iterator in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    if Range.contains_loc expr.exp_loc pos
    then (
      match expr.exp_desc with
      | Texp_let
          ( Nonrecursive
          , [ { vb_pat = { pat_desc = Tpat_var (inlined_var, { loc; _ }, _); _ }
              ; vb_expr = inlined_expr
              ; _
              }
            ]
          , _ )
        when Range.contains_loc loc pos ->
        raise_notrace (Found { inlined_var; inlined_expr })
      | _ -> I.default_iterator.expr iter expr)
  in
  let structure_item_iter (iter : I.iterator) (item : Typedtree.structure_item) =
    if Range.contains_loc item.str_loc pos
    then (
      match item.str_desc with
      | Tstr_value
          ( Nonrecursive
          , [ { vb_pat = { pat_desc = Tpat_var (inlined_var, { loc; _ }, _); _ }
              ; vb_expr = inlined_expr
              ; _
              }
            ] )
        when Range.contains_loc loc pos ->
        raise_notrace (Found { inlined_var; inlined_expr })
      | _ -> I.default_iterator.structure_item iter item)
  in
  let iterator =
    { I.default_iterator with expr = expr_iter; structure_item = structure_item_iter }
  in
  try
    iterator.structure iterator typedtree;
    None
  with
  | Found task -> Some task
;;

(** [find_parsetree_loc pl loc] finds an expression node in the parsetree with
    location [loc] *)
let find_parsetree_loc pipeline loc =
  let exception Found of Parsetree.expression in
  try
    let expr_iter (iter : Ast_iterator.iterator) (expr : Parsetree.expression) =
      if Loc.compare expr.pexp_loc loc = 0
      then raise_notrace (Found expr)
      else Ast_iterator.default_iterator.expr iter expr
    in
    let iterator = { Ast_iterator.default_iterator with expr = expr_iter } in
    (match Mpipeline.reader_parsetree pipeline with
     | `Implementation s -> iterator.structure iterator s
     | `Interface _ -> ());
    None
  with
  | Found e -> Some e
;;

let find_parsetree_loc_exn pipeline loc =
  Option.value_exn (find_parsetree_loc pipeline loc)
;;

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
;;

(** Mapping from [Location.t] to [Path.t]. Computed from the typedtree. Useful
    for determining whether two parsetree identifiers refer to the same path. *)
module Paths : sig
  type t

  val of_typedtree : Typedtree.expression -> t
  val same_path : t -> Loc.t -> Loc.t -> bool
end = struct
  type t = Path.t Loc.Map.t

  let find t x = Loc.Map.find_opt x t

  let of_typedtree (expr : Typedtree.expression) =
    let module I = Ocaml_typing.Tast_iterator in
    let paths = ref Loc.Map.empty in
    let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
      match expr.exp_desc with
      | Texp_ident (path, { loc; _ }, _) ->
        paths := Loc.Map.add !paths ~key:loc ~data:path
      | _ -> I.default_iterator.expr iter expr
    in
    let pat_iter (type k) (iter : I.iterator) (pat : k Typedtree.general_pattern) =
      match pat.pat_desc with
      | Tpat_var (id, { loc; _ }, _) ->
        paths := Loc.Map.add !paths ~key:loc ~data:(Path.Pident id)
      | Tpat_alias (pat, id, { loc; _ }, _, _) ->
        paths := Loc.Map.add !paths ~key:loc ~data:(Path.Pident id);
        I.default_iterator.pat iter pat
      | _ -> I.default_iterator.pat iter pat
    in
    let iterator = { I.default_iterator with expr = expr_iter; pat = pat_iter } in
    iterator.expr iterator expr;
    !paths
  ;;

  let same_path ps l l' =
    match find ps l, find ps l' with
    | Some p, Some p' -> Path.same p p'
    | _ -> false
  ;;
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
;;

(** Rough check for expressions that can be duplicated without duplicating any
    side effects (or introducing a sigificant performance difference). *)
let rec is_pure (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_ident _ | Pexp_constant _ | Pexp_unreachable -> true
  | Pexp_field (e, _) | Pexp_constraint (e, _) -> is_pure e
  | _ -> false
;;

let all_unlabeled_params =
  List.for_all ~f:(fun p ->
    match p.Parsetree.pparam_desc with
    | Pparam_val (Nolabel, _, _) -> true
    | _ -> false)
;;

let same_path paths (id : _ H.with_loc) (id' : _ H.with_loc) =
  Paths.same_path paths id.loc id'.loc
;;

let beta_reduce (paths : Paths.t) (app : Parsetree.expression) =
  let rec beta_reduce_arg body (pat : Parsetree.pattern) arg =
    let with_let () = H.Exp.let_ Nonrecursive [ H.Vb.mk pat arg ] body in
    let with_subst param = subst (same_path paths) arg param body in
    match pat.ppat_desc with
    | Ppat_any | Ppat_construct ({ txt = Lident "()"; _ }, _) ->
      if is_pure arg then body else with_let ()
    | Ppat_var param | Ppat_constraint ({ ppat_desc = Ppat_var param; _ }, _) ->
      if is_pure arg then with_subst param else with_let ()
    | Ppat_tuple (pats, _) ->
      let pats = List.map ~f:snd pats in
      (match arg.pexp_desc with
       | Pexp_tuple args ->
         let args = List.map ~f:snd args in
         List.fold2_exn pats args ~init:body ~f:beta_reduce_arg
       | _ -> with_let ())
    | _ -> with_let ()
  in
  let extract_param_pats params =
    List.map params ~f:(fun p ->
      match p.Parsetree.pparam_desc with
      | Pparam_val (Nolabel, _, pat) -> Some pat
      | _ -> None)
    |> Option.all
  in
  match app.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_function (params, None, Pfunction_body body); _ }, args)
    when List.length params = List.length args && all_unlabeled_params params ->
    (match extract_param_pats params with
     | Some pats ->
       List.fold2_exn pats args ~init:body ~f:(fun body pat (_, arg) ->
         beta_reduce_arg body pat arg)
     | None -> app)
  | _ -> app
;;

let inlined_text pipeline task =
  let open Option.O in
  let+ expr = find_parsetree_loc pipeline task.inlined_expr.exp_loc in
  let expr = strip_attribute "merlin.loc" expr in
  Format.asprintf "(%a)" Pprintast.expression expr
;;

type inline_use =
  | Application of Typedtree.expression
  | Identifier of Loc.t
  | Labelled_argument of string * Loc.t

let iter_inline_uses pipeline task ~f =
  let module I = Ocaml_typing.Tast_iterator in
  (* Inlining into an argument context has some special cases. *)
  let arg_iter
        env
        (iter : I.iterator)
        (label : Asttypes.arg_label)
        (m_arg_expr : Typedtree.apply_arg)
    =
    match label, m_arg_expr with
    (* Handle the labeled argument shorthand [f ~x] when inlining [x]. *)
    | Labelled name, Arg ({ exp_desc = Texp_ident (Pident id, { loc; _ }, _); _ } as arg)
      when Ident.same task.inlined_var id ->
      if not (f env (Labelled_argument (name, loc))) then iter.expr iter arg
    (* Inlining is allowed for optional arguments that are being passed a [Some]
       parameter, i.e. [x] may be inlined in [let x = 1 in
       (fun ?(x = 0) -> x) ~x]. *)
    | ( Optional name
      , Arg
          ({ exp_desc =
               (* The construct is part of desugaring and is assumed to be [Some]. *)
               Texp_construct
                 (_, _, [ { exp_desc = Texp_ident (Pident id, { loc; _ }, _); _ } ])
           ; _
           } as arg) )
      when Ident.same task.inlined_var id ->
      if not (f env (Labelled_argument (name, loc))) then iter.expr iter arg
    | Optional _, Arg ({ exp_desc = Texp_construct _; _ } as arg) -> iter.expr iter arg
    (* Inlining is not allowed for optional arguments that are being passed an
       optional parameter, i.e. [x] may not be inlined in [let x = Some 1 in
       (fun ?(x = 0) -> x) ?x]. *)
    | Optional _, Arg _ -> ()
    | _, Arg arg -> iter.expr iter arg
    | _, _ -> ()
  in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    (* When inlining into an application context, replace the entire application
       so that the result can be beta reduced. *)
    | Texp_apply (({ exp_desc = Texp_ident (Pident id, _, _); _ } as func), args)
      when Ident.same task.inlined_var id ->
      if not (f expr.exp_env (Application expr))
      then (
        iter.expr iter func;
        List.iter args ~f:(fun (label, arg) -> arg_iter expr.exp_env iter label arg))
    | Texp_apply (func, args) ->
      iter.expr iter func;
      List.iter args ~f:(fun (label, arg) -> arg_iter expr.exp_env iter label arg)
    | Texp_ident (Pident id, { loc; _ }, _) when Ident.same task.inlined_var id ->
      ignore (f expr.exp_env (Identifier loc) : bool)
    | _ -> I.default_iterator.expr iter expr
  in
  let iterator = { I.default_iterator with expr = expr_iter } in
  match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
  | `Interface _ -> ()
  | `Implementation structure -> iterator.structure iterator structure
;;

let inline_applicability pipeline task =
  let exception Applicable in
  let error = ref None in
  let check_use env (_ : inline_use) =
    match check_shadowing task.inlined_expr env with
    | Ok () -> raise_notrace Applicable
    | Error e ->
      error := Some e;
      false
  in
  match
    iter_inline_uses pipeline task ~f:check_use;
    !error
  with
  | None -> `Not_applicable
  | Some error -> `Disabled error
  | exception Applicable -> `Applicable
;;

(** [inline_edits pipeline task] returns a list of inlining edits and an
    optional error value. An error will be generated if any of the potential
    inlinings is not allowed due to shadowing. The successful edits will still
    be returned *)
let inline_edits pipeline task =
  let open Option.O in
  let+ newText = inlined_text pipeline task in
  let make_edit newText loc = TextEdit.create ~newText ~range:(Range.of_loc loc) in
  let edits = Queue.create () in
  let error = ref None in
  let insert_edit newText loc = Queue.enqueue edits (make_edit newText loc) in
  let shadowed env =
    match check_shadowing task.inlined_expr env with
    | Ok () -> false
    | Error e ->
      error := Some e;
      true
  in
  let paths = Paths.of_typedtree task.inlined_expr in
  let inlined_pexpr = find_parsetree_loc_exn pipeline task.inlined_expr.exp_loc in
  let inline_use env use =
    if shadowed env
    then false
    else (
      (match use with
       | Identifier loc -> insert_edit newText loc
       | Labelled_argument (name, loc) -> insert_edit (sprintf "%s:%s" name newText) loc
       | Application expr ->
         let newText =
           let reduced_pexpr =
             let app_pexpr = find_parsetree_loc_exn pipeline expr.exp_loc in
             match app_pexpr.pexp_desc with
             | Pexp_apply ({ pexp_desc = Pexp_ident _; _ }, args) ->
               beta_reduce paths (H.Exp.apply inlined_pexpr args)
             | _ -> app_pexpr
           in
           Format.asprintf "(%a)" Pprintast.expression
           @@ strip_attribute "merlin.loc" reduced_pexpr
         in
         insert_edit newText expr.exp_loc);
      true)
  in
  iter_inline_uses pipeline task ~f:inline_use;
  Queue.to_list edits, !error
;;

let inline_task pipeline (range : Range.t) =
  let open Option.O in
  let* typedtree =
    match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
    | `Interface _ -> None
    | `Implementation x -> Some x
  in
  find_inline_task typedtree range.start
;;

let disabled_code_action error =
  CodeAction.create
    ~title:action_title
    ~kind:RefactorInline
    ~isPreferred:false
    ~disabled:(CodeActionDisabled.create ~reason:(string_of_error error))
    ()
;;

let code_action_for_task pipeline doc task =
  let open Option.O in
  let* edits, m_error = inline_edits pipeline task in
  match edits, m_error with
  | [], None -> None
  | [], Some error -> Some (disabled_code_action error)
  | _ :: _, (Some _ | None) ->
    let action =
      let edit = Text_document.workspace_edit (Document.text_document doc) edits in
      CodeAction.create
        ~title:action_title
        ~kind:RefactorInline
        ~edit
        ~isPreferred:false
        ()
    in
    Some action
;;

module Resolve_data = struct
  let action = "inline"

  type t =
    { uri : DocumentUri.t
    ; range : Range.t
    ; version : int
    }

  let create doc range =
    `Assoc
      [ "action", `String action
      ; "uri", DocumentUri.yojson_of_t (Document.uri doc)
      ; "range", Range.yojson_of_t range
      ; "version", `Int (Document.version doc)
      ]
  ;;

  let is_inline = function
    | `Assoc fields ->
      (match Json.field fields "action" Json.Conv.string_of_yojson with
       | Some value -> String.equal value action
       | None -> false)
    | _ -> false
  ;;

  let of_json = function
    | `Assoc fields ->
      let uri = Json.field_exn fields "uri" DocumentUri.t_of_yojson in
      let range = Json.field_exn fields "range" Range.t_of_yojson in
      let version =
        Json.field_exn fields "version" (function
          | `Int version -> version
          | json -> Json.error "expected an integer" json)
      in
      { uri; range; version }
    | json -> Json.error "invalid inline code action data" json
  ;;
end

let unresolved_code_action pipeline doc (params : CodeActionParams.t) =
  let open Option.O in
  let* task = inline_task pipeline params.range in
  (* Only edit construction is deferred: suppress unusable actions and determine
     [disabled] eagerly because the client may not offer to resolve that property. *)
  match inline_applicability pipeline task with
  | `Not_applicable -> None
  | `Disabled error -> Some (disabled_code_action error)
  | `Applicable ->
    let data = Resolve_data.create doc params.range in
    Some
      (CodeAction.create
         ~title:action_title
         ~kind:CodeActionKind.RefactorInline
         ~data
         ~isPreferred:false
         ())
;;

let content_modified () =
  Jsonrpc.Response.Error.make
    ~code:Jsonrpc.Response.Error.Code.ContentModified
    ~message:"The document changed before the code action was resolved"
    ()
  |> Jsonrpc.Response.Error.raise
;;

let resolve (state : State.t) (action : CodeAction.t) =
  match action.data with
  | None -> None
  | Some data when not (Resolve_data.is_inline data) -> None
  | Some data ->
    let { Resolve_data.uri; range; version } = Resolve_data.of_json data in
    let resolve () =
      (let doc =
         match Document_store.get_opt state.store uri with
         | Some doc when Int.equal (Document.version doc) version -> doc
         | None | Some _ -> content_modified ()
       in
       let merlin = Document.merlin_exn doc in
       Document.Merlin.with_pipeline_exn ~name:"resolve-inline-code-action" merlin
       @@ fun pipeline ->
       inline_task pipeline range |> Option.bind ~f:(code_action_for_task pipeline doc))
      >>| function
      | None -> content_modified ()
      | Some resolved -> { resolved with data = action.data }
    in
    Some (resolve ())
;;

let code_action pipeline doc (params : CodeActionParams.t) =
  inline_task pipeline params.range |> Option.bind ~f:(code_action_for_task pipeline doc)
;;

let t = Code_action.batchable RefactorInline code_action
let unresolved = Code_action.batchable RefactorInline unresolved_code_action
