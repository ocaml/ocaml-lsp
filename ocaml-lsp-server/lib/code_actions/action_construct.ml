open Import

let action_kind = "construct"

let code_action doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | Intf -> Fiber.return None
  | Impl ->
    let pos = Position.logical params.range.Range.end_ in
    (* we want this predicate to quickly eliminate prefixes that don't fit to be
       a hole *)
    let prefix =
      let src = Document.source doc in
      Compl.prefix_of_position ~short_path:false src pos
    in
    if not (Typed_hole.can_be_hole prefix) then
      Fiber.return None
    else
      let open Fiber.O in
      let+ structures =
        Document.with_pipeline_exn doc (fun pipeline ->
            let typedtree =
              let typer = Mpipeline.typer_result pipeline in
              Mtyper.get_typedtree typer
            in
            let pos = Mpipeline.get_lexing_pos pipeline pos in
            Mbrowse.enclosing pos [ Mbrowse.of_typedtree typedtree ])
      in
      if not (Typed_hole.is_a_hole structures) then
        None
      else
        (* ocaml-lsp can provide [Construct] values as completion entries, so
           this code action requests the client [1] to trigger completion
           request at the cursor's current position [2]

           this code action simply requests the client [1] to trigger completion
           suggestions, as ocaml-lsp provides constructed values as completion
           entries, when the cursor is on a typed hole [2]

           [1] currently, the command is vscode-specific, so only vscode client
           is supported (note: this _code action_ works only for vscode;
           [Construct] support works for all clients)

           [2] vscode doesn't provide API to show specific completion entries,
           only a command to trigger all completion providers at the current
           cursor position *)
        let code_action =
          CodeAction.create ~title:"Construct an expression"
            ~kind:(CodeActionKind.Other action_kind)
            ~command:Client.Vscode.Commands.triggerSuggest ()
        in
        Some code_action
