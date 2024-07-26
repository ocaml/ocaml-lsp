open Import
open Fiber.O

let rename (state : State.t) { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position), `Buffer)
    in
    let+ locs, _desync = Document.Merlin.dispatch_exn ~name:"rename" merlin command in
    let version = Document.version doc in
    let source = Document.source doc in
    let edits =
      List.map locs ~f:(fun (loc : Warnings.loc) ->
        let range = Range.of_loc loc in
        let make_edit () = TextEdit.create ~range ~newText:newName in
        match
          let occur_start_pos =
            Position.of_lexical_position loc.loc_start |> Option.value_exn
          in
          occur_start_pos
        with
        | { character = 0; _ } -> make_edit ()
        | pos ->
          let mpos = Position.logical pos in
          let (`Offset index) = Msource.get_offset source mpos in
          assert (index > 0)
          (* [index = 0] if we pass [`Logical (1, 0)], but we handle the case
             when [character = 0] in a separate matching branch *);
          let source_txt = Msource.text source in
          (match source_txt.[index - 1] with
           | '~' (* the occurrence is a named argument *)
           | '?' (* is an optional argument *) ->
             let empty_range_at_occur_end =
               let occur_end_pos = range.Range.end_ in
               { range with start = occur_end_pos }
             in
             TextEdit.create ~range:empty_range_at_occur_end ~newText:(":" ^ newName)
           | _ -> make_edit ()))
    in
    let workspace_edits =
      let documentChanges =
        let open Option.O in
        Option.value
          ~default:false
          (let client_capabilities = State.client_capabilities state in
           let* workspace = client_capabilities.workspace in
           let* edit = workspace.workspaceEdit in
           edit.documentChanges)
      in
      if documentChanges
      then (
        let textDocument =
          OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
        in
        let edits = List.map edits ~f:(fun e -> `TextEdit e) in
        WorkspaceEdit.create
          ~documentChanges:
            [ `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits) ]
          ())
      else WorkspaceEdit.create ~changes:[ uri, edits ] ()
    in
    workspace_edits
;;
