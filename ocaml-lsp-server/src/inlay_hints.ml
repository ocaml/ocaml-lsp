open Import
open Fiber.O

let overlaps (x : Range.t) (y : Range.t) =
  let open Ordering in
  match (Position.compare x.start y.end_, Position.compare x.end_ y.start) with
  | (Lt | Eq), (Gt | Eq) | (Gt | Eq), (Lt | Eq) -> true
  | _ -> false

let range_overlaps_loc range loc =
  match Range.of_loc_opt loc with
  | Some range' -> overlaps range range'
  | None -> false

let outline_type ~env typ =
  let ppf, to_string = Format.to_string () in
  Ocaml_typing.Printtyp.wrap_printing_env env (fun () ->
      Merlin_analysis.Type_utils.print_type_with_decl
        ~verbosity:(Mconfig.Verbosity.Lvl 0)
        env
        ppf
        typ);
  Some (sprintf ": %s" (to_string ()))

let hint_pattern_iter typedtree range k =
  let module I = Ocaml_typing.Tast_iterator in
  let expr iter (e : Typedtree.expression) =
    if range_overlaps_loc range e.exp_loc then I.default_iterator.expr iter e;
    match e.exp_desc with
    | Texp_let (_, vbs, _) ->
      List.iter vbs ~f:(fun (vb : Typedtree.value_binding) ->
          if range_overlaps_loc range vb.vb_loc then k vb.vb_pat)
    | Texp_letop { body; _ } ->
      if range_overlaps_loc range body.c_lhs.pat_loc then k body.c_lhs
    | _ -> ()
  in

  let structure_item iter (item : Typedtree.structure_item) =
    if range_overlaps_loc range item.str_loc then
      I.default_iterator.structure_item iter item
  in
  let iterator = { I.default_iterator with expr; structure_item } in
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
            hint_pattern_iter typedtree range (fun pat ->
                let open Option.O in
                let hint =
                  let* label = outline_type ~env:pat.pat_env pat.pat_type in
                  let+ position =
                    Position.of_lexical_position pat.pat_loc.loc_end
                  in
                  InlayHint.create
                    ~kind:Type
                    ~position
                    ~label:(`String label)
                    ~paddingLeft:true
                    ~paddingRight:true
                    ()
                in
                Option.iter hint ~f:(fun hint -> hints := hint :: !hints)))
    in
    Fiber.return (Some !hints)
