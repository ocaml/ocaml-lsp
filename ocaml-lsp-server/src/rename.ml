open Import
open Fiber.O

let rename (state : State.t) { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let command =
      Query_protocol.Occurrences
        (`Ident_at (Document.merlin_position doc position :> Msource.position), `Renaming)
    in
    let+ occurrences, _desync =
      Document.Merlin.dispatch_exn ~name:"rename" merlin command
    in
    let locs =
      List.filter_map occurrences ~f:(fun (occurrence : Query_protocol.occurrence) ->
        match occurrence.is_stale with
        | true -> None
        | false -> Some occurrence.loc)
    in
    let version = Document.version doc in
    let uri = Document.uri doc in
    let locs =
      List.fold_left locs ~init:Uri.Map.empty ~f:(fun acc (loc : Warnings.loc) ->
        let uri =
          match loc.loc_start.pos_fname with
          | "" -> uri
          | path -> Uri.of_path path
        in
        Uri.Map.add_to_list uri loc acc)
    in
    let edits =
      Uri.Map.mapi
        (fun doc_uri locs ->
           let target_doc = Document_store.get_opt state.store doc_uri in
           let source =
             match target_doc with
             | Some doc -> Document.source doc
             | None ->
               let source_path = Uri.to_path doc_uri in
               In_channel.with_open_text source_path In_channel.input_all |> Msource.make
           in
           List.map locs ~f:(fun (loc : Warnings.loc) ->
             let range =
               match target_doc with
               | Some doc -> Document.range_of_loc doc loc
               | None -> Document.range_of_loc_in_source doc source loc
             in
             let edit = TextEdit.create ~range ~newText:newName in
             let byte_character = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
             if byte_character = 0
             then edit
             else (
               let mpos = `Logical (loc.loc_start.pos_lnum, byte_character) in
               let (`Offset index) = Msource.get_offset source mpos in
               assert (index > 0);
               let source_txt = Msource.text source in
               (* TODO: handle record field punning *)
               match source_txt.[index - 1] with
               | '~' (* the occurrence is a named argument *)
               | '?' (* is an optional argument *) ->
                 let empty_range_at_occur_end =
                   let occur_end_pos = edit.range.end_ in
                   { edit.range with start = occur_end_pos }
                 in
                 TextEdit.create ~range:empty_range_at_occur_end ~newText:(":" ^ newName)
               | _ -> edit)))
        locs
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
