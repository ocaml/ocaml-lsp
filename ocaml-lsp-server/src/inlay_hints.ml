open Import
open Fiber.O

let range_overlaps_loc range loc =
  match Range.of_loc_opt loc with
  | Some range' -> Range.overlaps range range'
  | None -> false

let outline_type ~env typ =
  Ocaml_typing.Printtyp.wrap_printing_env env (fun () ->
      Format.asprintf "@[<h>: %a@]" Ocaml_typing.Printtyp.type_scheme typ)
  |> String.extract_words ~is_word_char:(function
         | ' ' | '\t' | '\n' -> false
         | _ -> true)
  |> String.concat ~sep:" "

let hint_binding_iter typedtree range k =
  let module I = Ocaml_typing.Tast_iterator in
  let expr (iter : I.iterator) (e : Typedtree.expression) =
    if range_overlaps_loc range e.exp_loc then
      match e.exp_desc with
      | Texp_function
          { arg_label = Optional _
          ; cases =
              [ { c_rhs =
                    { exp_desc = Texp_let (_, [ { vb_pat; _ } ], body); _ }
                ; _
                }
              ]
          ; _
          } ->
        iter.pat iter vb_pat;
        iter.expr iter body
      | _ -> I.default_iterator.expr iter e
  in

  let structure_item (iter : I.iterator) (item : Typedtree.structure_item) =
    if range_overlaps_loc range item.str_loc then
      I.default_iterator.structure_item iter item
  in
  let value_binding (iter : I.iterator) (vb : Typedtree.value_binding) =
    if range_overlaps_loc range vb.vb_loc then
      match vb.vb_expr.exp_desc with
      | Texp_function _ -> iter.expr iter vb.vb_expr
      | _ -> I.default_iterator.value_binding iter vb
  in
  let pat (type k) iter (pat : k Typedtree.general_pattern) =
    if range_overlaps_loc range pat.pat_loc then
      let has_constraint =
        List.exists pat.pat_extra ~f:(fun (extra, _, _) ->
            match extra with
            | Typedtree.Tpat_constraint _ -> true
            | _ -> false)
      in
      if not has_constraint then (
        I.default_iterator.pat iter pat;
        match pat.pat_desc with
        | Tpat_var _ when not pat.pat_loc.loc_ghost ->
          k pat.pat_env pat.pat_type pat.pat_loc
        | _ -> ())
  in
  let iterator =
    { I.default_iterator with expr; structure_item; pat; value_binding }
  in
  iterator.structure iterator typedtree

let compute (state : State.t)
    { InlayHintParams.range; textDocument = { uri }; _ } =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return None
  | `Merlin doc ->
    let hints = ref [] in
    let* () =
      Document.Merlin.with_pipeline_exn doc (fun pipeline ->
          match Mtyper.get_typedtree (Mpipeline.typer_result pipeline) with
          | `Interface _ -> ()
          | `Implementation typedtree ->
            hint_binding_iter typedtree range (fun env type_ loc ->
                let open Option.O in
                let hint =
                  let label = outline_type ~env type_ in
                  let+ position = Position.of_lexical_position loc.loc_end in
                  InlayHint.create
                    ~kind:Type
                    ~position
                    ~label:(`String label)
                    ~paddingLeft:false
                    ~paddingRight:false
                    ()
                in
                Option.iter hint ~f:(fun hint -> hints := hint :: !hints)))
    in
    Fiber.return (Some !hints)
