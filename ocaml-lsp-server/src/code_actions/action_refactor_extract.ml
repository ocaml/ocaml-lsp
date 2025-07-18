open Import

let action_kind = "refactor-extract (extract an area into a fresh let binding)"

let make_edit params doc { Query_protocol.loc; content; selection_range = _ } =
  let uri = params.CodeActionParams.textDocument.uri in
  let textDocument =
    OptionalVersionedTextDocumentIdentifier.create ~uri ~version:(Document.version doc) ()
  in
  let textedit = TextEdit.create ~newText:content ~range:(Range.of_loc loc) in
  let edit = TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ] in
  WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
;;

let dispatch_command pipeline doc ~start ~stop =
  let buffer = Document.source doc in
  let command = Query_protocol.Refactor_extract_region (start, stop, None, buffer) in
  Query_commands.dispatch pipeline command
;;

let code_action doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return None
  | `Merlin merlin ->
    let start = Position.logical params.range.Range.start in
    let stop = Position.logical params.range.Range.end_ in
    Document.Merlin.with_pipeline_exn ~name:"refactor" merlin (fun pipeline ->
      let typer = Mpipeline.typer_result pipeline in
      let typedtree = Mtyper.get_typedtree typer in
      match typedtree with
      | `Interface _ -> None
      | `Implementation structure ->
        let enclosing =
          Mbrowse.enclosing
            (Mpipeline.get_lexing_pos pipeline start)
            [ Mbrowse.of_typedtree typedtree ]
        in
        if
          Merlin_analysis.Refactor_extract_region.is_region_extractable
            ~start:(Mpipeline.get_lexing_pos pipeline start)
            ~stop:(Mpipeline.get_lexing_pos pipeline stop)
            enclosing
            structure
        then (
          let substitution = dispatch_command pipeline doc ~start ~stop in
          let edit = make_edit params doc substitution in
          let code_action =
            CodeAction.create
              ~title:"Extract expression"
              ~kind:(CodeActionKind.Other action_kind)
              ~edit
              ()
          in
          Some code_action)
        else None)
;;

let t = Code_action.non_batchable (Other action_kind) code_action
