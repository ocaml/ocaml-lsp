open Import

let action_kind = "construct"

let code_action doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | Intf -> Fiber.return None
  | Impl -> (
    let src = Document.source doc in
    let pos = Position.logical params.range.Range.end_ in
    (* we want this predicate to quickly eliminate prefixes that don't fit to be
       a hole *)
    let can_be_hole =
      let prefix = Compl.prefix_of_position ~short_path:false src pos in
      let hole_syntax = "_" in
      String.equal prefix hole_syntax
    in
    if not can_be_hole then
      Fiber.return None
    else
      let code_action =
        let command : Command.t =
          Command.create ~title:"Trigger Suggest"
            ~command:"editor.action.triggerSuggest" ()
        in
        CodeAction.create ~title:"Construct an expression"
          ~kind:(CodeActionKind.Other action_kind) ~command ()
      in
      Document.with_pipeline_exn doc @@ fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      let typedtree = Mtyper.get_typedtree typer in
      let pos = Mpipeline.get_lexing_pos pipeline pos in
      let structures =
        Mbrowse.enclosing pos [ Mbrowse.of_typedtree typedtree ]
      in
      (* the pattern matching below is taken and modified from
         [Query_commands.dispatch]'s [Construct] branch;

         If we directly dispatched [Construct] command to merlin, we'd be doing
         useless computations: we need info whether the expression at the cursor
         is a hole, we don't need constructed expressions yet.

         Ideally, merlin should return a callback option, which is some when the
         context is applicable. *)
      match structures with
      | (_, Browse_raw.Module_expr { mod_desc = Tmod_hole; _ }) :: (_, _) :: _
        ->
        Some code_action
      | (_, Browse_raw.Expression { exp_desc = Texp_hole; _ }) :: _ ->
        Some code_action
      | []
      | (_, _) :: _ ->
        None)
