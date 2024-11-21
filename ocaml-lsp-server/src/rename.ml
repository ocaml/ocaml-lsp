open Import
open Fiber.O

let rename (state : State.t) { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position), `Renaming)
    in
    let+ locs, _desync = Document.Merlin.dispatch_exn ~name:"rename" merlin command in
    let version = Document.version doc in
    let uri = Document.uri doc in
    let edits =
      List.fold_left locs ~init:Uri.Map.empty ~f:(fun acc (loc : Warnings.loc) ->
        let range = Range.of_loc loc in
        let edit = TextEdit.create ~range ~newText:newName in
        let uri =
          match loc.loc_start.pos_fname with
          | "" -> uri
          | path -> Uri.of_path path
        in
        Uri.Map.add_to_list uri edit acc)
    in
    let edits =
      Uri.Map.mapi
        (fun doc_uri edits ->
           let source =
             match Document_store.get_opt state.store doc_uri with
             | Some doc when DocumentUri.equal doc_uri (Document.uri doc) ->
               Document.source doc
             | Some _ | None ->
               let source_path = Uri.to_path doc_uri in
               In_channel.with_open_text source_path In_channel.input_all |> Msource.make
           in
           List.map edits ~f:(fun (edit : TextEdit.t) ->
             let start_position = edit.range.start in
             match start_position with
             | { character = 0; _ } -> edit
             | pos ->
               let mpos = Position.logical pos in
               let (`Offset index) = Msource.get_offset source mpos in
               assert (index > 0)
               (* [index = 0] if we pass [`Logical (1, 0)], but we handle the case
                 when [character = 0] in a separate matching branch *);
               let source_txt = Msource.text source in
               (* TODO: handle record field puning *)
               (match source_txt.[index - 1] with
                | '~' (* the occurrence is a named argument *)
                | '?' (* is an optional argument *) ->
                  let empty_range_at_occur_end =
                    let occur_end_pos = edit.range.end_ in
                    { edit.range with start = occur_end_pos }
                  in
                  TextEdit.create ~range:empty_range_at_occur_end ~newText:(":" ^ newName)
                | _ -> edit)))
        edits
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
        let documentChanges =
          Uri.Map.to_list edits
          |> List.map ~f:(fun (uri, edits) ->
            let textDocument =
              OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
            in
            let edits = List.map edits ~f:(fun e -> `TextEdit e) in
            `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits))
        in
        WorkspaceEdit.create ~documentChanges ())
      else (
        let changes = Uri.Map.to_list edits in
        WorkspaceEdit.create ~changes ())
    in
    workspace_edits
;;
