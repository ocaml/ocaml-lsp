open Import
open Fiber.O

let range_overlaps_loc range loc =
  match Range.of_loc_opt loc with
  | Some range' -> Range.overlaps range range'
  | None -> false
;;

let outline_type ~env typ =
  Ocaml_typing.Printtyp.wrap_printing_env env (fun () ->
    Format.asprintf "@[<h>: %a@]" Ocaml_typing.Printtyp.type_scheme typ)
  |> String.extract_words ~is_word_char:(function
    | ' ' | '\t' | '\n' -> false
    | _ -> true)
  |> String.concat ~sep:" "
;;

let hint_binding_iter
  ?(hint_let_bindings = false)
  ?(hint_pattern_variables = false)
  ?(hint_function_params = false)
  typedtree
  range
  k
  =
  let module I = Ocaml_typing.Tast_iterator in
  (* to be used for pattern variables in match cases, but not for function
     arguments *)
  let case hint_lhs (iter : I.iterator) (case : _ Typedtree.case) =
    if hint_lhs then iter.pat iter case.c_lhs;
    Option.iter case.c_guard ~f:(iter.expr iter);
    iter.expr iter case.c_rhs
  in
  let value_binding hint_lhs (iter : I.iterator) (vb : Typedtree.value_binding) =
    if range_overlaps_loc range vb.vb_loc
    then
      if not hint_lhs
      then iter.expr iter vb.vb_expr
      else (
        match vb.vb_expr.exp_desc with
        | Texp_function _ -> iter.expr iter vb.vb_expr
        | _ -> I.default_iterator.value_binding iter vb)
  in
  let expr (iter : I.iterator) (e : Typedtree.expression) =
    if range_overlaps_loc range e.exp_loc
    then (
      match e.exp_desc with
      | Texp_function { body; params; _ } ->
        (match body with
         | Tfunction_cases { fc_cases; _ } ->
           if hint_function_params
           then
             List.iter params ~f:(fun (param : Typedtree.function_param) ->
               match param.fp_kind with
               | Tparam_pat pat -> iter.pat iter pat
               | Tparam_optional_default (pat, _, _) -> iter.pat iter pat);
           List.iter fc_cases ~f:(fun { Typedtree.c_lhs; c_rhs; _ } ->
             if hint_pattern_variables then iter.pat iter c_lhs;
             iter.expr iter c_rhs)
         | Tfunction_body body when not hint_function_params -> iter.expr iter body
         | _ -> I.default_iterator.expr iter e)
      | Texp_let (_, vbs, body) ->
        List.iter vbs ~f:(value_binding hint_let_bindings iter);
        iter.expr iter body
      | Texp_letop { body; _ } -> case hint_let_bindings iter body
      | Texp_match (expr, _, cases, _) ->
        iter.expr iter expr;
        List.iter cases ~f:(case hint_pattern_variables iter)
      (* Stop iterating when we see a ghost location to avoid annotating generated code *)
      | _ when e.exp_loc.loc_ghost && not inside_test -> ()
      | _ -> I.default_iterator.expr iter e)
  in
  let structure_item (iter : I.iterator) (item : Typedtree.structure_item) =
    if range_overlaps_loc range item.str_loc
    then (
      match item.str_desc with
      | Typedtree.Tstr_value (_, vbs) ->
        List.iter vbs ~f:(fun (vb : Typedtree.value_binding) -> expr iter vb.vb_expr)
      (* Stop iterating when we see a ghost location to avoid annotating generated code *)
      | _ when item.str_loc.loc_ghost && not inside_test -> ()
      | _ -> I.default_iterator.structure_item iter item)
  in
  let pat (type k) iter (pat : k Typedtree.general_pattern) =
    if range_overlaps_loc range pat.pat_loc
    then (
      let has_constraint =
        List.exists pat.pat_extra ~f:(fun (extra, _, _) ->
          match extra with
          | Typedtree.Tpat_constraint _ -> true
          | _ -> false)
      in
      if not has_constraint
      then (
        I.default_iterator.pat iter pat;
        match pat.pat_desc with
        | Tpat_var _ when not pat.pat_loc.loc_ghost ->
          k pat.pat_env pat.pat_type pat.pat_loc
        | _ -> ()))
  in
  let iterator =
    { I.default_iterator with
      expr
    ; structure_item
    ; pat
    ; value_binding = value_binding true
    }
  in
  iterator.structure iterator typedtree
;;

let let_syntax_at typer pos =
  let drop_library_name_if_in_scope
    (env : Env.t)
    (decl : Types.module_declaration)
    (path : Path.t)
    =
    let rec to_lident (path : Path.t) : Longident.t =
      match path with
      | Pident ident -> Lident (Ident.name ident)
      | Pdot (path, name) -> Ldot (to_lident path, name)
      | Papply (lhs, rhs) -> Lapply (to_lident lhs, to_lident rhs)
      | Pextra_ty (path, _) -> to_lident path
    in
    let rec drop_libname (path : Path.t) : Longident.t option =
      match path with
      | Pident _ -> None
      | Pdot (Pident _, name) -> Some (Lident name)
      | Pdot (path, name) ->
        Option.map (drop_libname path) ~f:(fun ident -> Longident.Ldot (ident, name))
      | Papply _ | Pextra_ty _ -> None
    in
    match drop_libname path with
    | Some ident ->
      (try
         let let_syntax : Longident.t = Ldot (Ldot (ident, "Let_syntax"), "Let_syntax") in
         let _, decl' = Env.find_module_by_name let_syntax env in
         if Ocaml_typing.Shape.Uid.equal decl.md_uid decl'.md_uid
         then ident
         else to_lident path
       with
       | _ -> to_lident path)
    | None -> to_lident path
  in
  List.find_map (Mtyper.node_at typer pos) ~f:(fun (env, _) ->
    try
      let path, decl = Env.find_module_by_name (Lident "Let_syntax") env in
      match path with
      | Pdot (Pdot (path, "Let_syntax"), "Let_syntax") ->
        Some (drop_library_name_if_in_scope env decl path)
      | single_let_syntax ->
        (* The [Let_syntax] module is avialable via an alias (e.g. like it would be when
           there is an [open Async] or similar). Try to resolve the original name of the
           module. *)
        let rec strip_let_syntax_suffix = function
          | Path.Pdot (path, "Let_syntax") -> strip_let_syntax_suffix path
          | path -> path
        in
        let normalized =
          strip_let_syntax_suffix (Env.normalize_module_path None env single_let_syntax)
        in
        let short =
          Ocaml_typing.Short_paths.find_module (Env.short_paths env) normalized
        in
        Some (drop_library_name_if_in_scope env decl short)
    with
    | _ -> None)
;;

let hint_let_syntax_ppx_iter typer parsetree range create_inlay_hint =
  let current_let_syntax = ref [] in
  let push_let_syntax pos =
    match let_syntax_at typer pos with
    | Some path ->
      let syntax = "." ^ Format.asprintf "%a" Pprintast.longident path in
      current_let_syntax := syntax :: !current_let_syntax
    | None ->
      (match !current_let_syntax with
       | hd :: _ -> current_let_syntax := hd :: !current_let_syntax
       | [] -> ())
  in
  let pop_let_syntax () =
    current_let_syntax
    := match !current_let_syntax with
       | [] -> []
       | _ :: tl -> tl
  in
  let structure (iter : Ast_iterator.iterator) (items : Parsetree.structure) =
    let prev_let_syntax = !current_let_syntax in
    let (_ : bool) =
      List.fold_left
        items
        ~init:false
        ~f:(fun should_push (item : Parsetree.structure_item) ->
          if should_push then push_let_syntax item.pstr_loc.loc_start;
          iter.structure_item iter item;
          match item.pstr_desc with
          | Pstr_open _ | Pstr_include _ -> true
          | _ -> false)
    in
    current_let_syntax := prev_let_syntax
  in
  let expr (iter : Ast_iterator.iterator) (expr : Parsetree.expression) =
    match expr.pexp_desc with
    | Pexp_open (decl, body) ->
      iter.open_declaration iter decl;
      push_let_syntax expr.pexp_loc.loc_start;
      iter.expr iter body;
      pop_let_syntax ()
    | Pexp_extension (name, payload) ->
      (* Generate annotation for [bind] and [map] extensions. *)
      (match name.txt with
       | "bind" | "map" ->
         (match !current_let_syntax with
          | syntax :: _ when range_overlaps_loc range name.loc ->
            create_inlay_hint syntax name.loc
          | _ -> ())
       | _ -> ());
      iter.payload iter payload
    | _ -> Ast_iterator.default_iterator.expr iter expr
  in
  let iterator = { Ast_iterator.default_iterator with structure; expr } in
  match parsetree with
  | `Interface signature -> iterator.signature iterator signature
  | `Implementation structure -> iterator.structure iterator structure
;;

let compute
  ~log_info
  (state : State.t)
  { InlayHintParams.range; textDocument = { uri }; _ }
  =
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return None
  | `Merlin doc ->
    let+ hints =
      let hint_let_bindings =
        Option.map state.configuration.data.inlay_hints ~f:(fun c -> c.hint_let_bindings)
      in
      let hint_pattern_variables =
        Option.map state.configuration.data.inlay_hints ~f:(fun c ->
          c.hint_pattern_variables)
      in
      let hint_function_params =
        Option.map state.configuration.data.inlay_hints ~f:(fun c ->
          c.hint_function_params)
      in
      let hint_let_syntax_ppx =
        Option.map state.configuration.data.inlay_hints ~f:(fun c ->
          c.hint_let_syntax_ppx)
      in
      Document.Merlin.with_pipeline_exn ~log_info doc (fun pipeline ->
        let hints = ref [] in
        (match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
         | `Interface _ -> ()
         | `Implementation typedtree ->
           hint_binding_iter
             ?hint_let_bindings
             ?hint_pattern_variables
             ?hint_function_params
             typedtree
             range
             (fun env type_ loc ->
                let hint =
                  let label = outline_type ~env type_ in
                  let open Option.O in
                  let+ position = Position.of_lexical_position loc.loc_end in
                  InlayHint.create
                    ~kind:Type
                    ~position
                    ~label:(`String label)
                    ~paddingLeft:false
                    ~paddingRight:false
                    ()
                in
                Option.iter hint ~f:(fun hint -> hints := hint :: !hints));
           if Option.value ~default:false hint_let_syntax_ppx
           then
             hint_let_syntax_ppx_iter
               (Mpipeline.typer_result pipeline)
               (Mpipeline.reader_parsetree pipeline)
               range
               (fun syntax loc ->
                  let open Option.O in
                  let hint =
                    let+ position = Position.of_lexical_position loc.loc_end in
                    InlayHint.create
                      ~kind:Type
                      ~position
                      ~label:(`String syntax)
                      ~paddingLeft:false
                      ~paddingRight:false
                      ()
                  in
                  Option.iter hint ~f:(fun hint -> hints := hint :: !hints)));
        !hints)
    in
    Some hints
;;
