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
      | Texp_function
          ( _
          , Tfunction_cases
              { cases =
                  [ { c_rhs = { exp_desc = Texp_let (_, [ { vb_pat; _ } ], body); _ }; _ }
                  ]
              ; _
              } ) ->
        iter.pat iter vb_pat;
        iter.expr iter body
      | Texp_let (_, vbs, body) ->
        List.iter vbs ~f:(value_binding hint_let_bindings iter);
        iter.expr iter body
      | Texp_letop { body; _ } -> case hint_let_bindings iter body
      | Texp_match (expr, cases, _) ->
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

let compute (state : State.t) { InlayHintParams.range; textDocument = { uri }; _ } =
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
      Document.Merlin.with_pipeline_exn ~name:"inlay-hints" doc (fun pipeline ->
        let hints = ref [] in
        (match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
         | `Interface _ -> ()
         | `Implementation typedtree ->
           hint_binding_iter
             ?hint_let_bindings
             ?hint_pattern_variables
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
                Option.iter hint ~f:(fun hint -> hints := hint :: !hints)));
        !hints)
    in
    Some hints
;;
