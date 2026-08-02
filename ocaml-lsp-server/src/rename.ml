open Import
open Fiber.O

let position_of_offset source offset =
  let (`Logical (line, character)) = Msource.get_logical source (`Offset offset) in
  Position.of_logical ~line ~character
;;

let compare_text_edit (a : TextEdit.t) (b : TextEdit.t) =
  match Range.compare a.range b.range with
  | 0 -> String.compare a.newText b.newText
  | ordering -> ordering
;;

let is_parenthesized source ~start_offset ~end_offset =
  end_offset - start_offset >= 3
  && Char.equal source.[start_offset] '('
  && Char.equal source.[end_offset - 1] ')'
;;

(* Merlin includes the syntactically required parentheses in symbolic operator
   locations. Rename only the operator so that clients do not use the
   parentheses as part of its name. *)
let identifier_range source loc =
  let range = Range.of_loc loc in
  let (`Offset start_offset) = Msource.get_offset source (Position.logical range.start) in
  let (`Offset end_offset) = Msource.get_offset source (Position.logical range.end_) in
  let source_text = Msource.text source in
  match is_parenthesized source_text ~start_offset ~end_offset with
  | false -> range
  | true ->
    let rec skip_whitespace_forward offset =
      if offset < end_offset - 1 && Char.is_whitespace source_text.[offset]
      then skip_whitespace_forward (offset + 1)
      else offset
    in
    let operator_start = skip_whitespace_forward (start_offset + 1) in
    let rec skip_whitespace_backward offset =
      if operator_start < offset && Char.is_whitespace source_text.[offset - 1]
      then skip_whitespace_backward (offset - 1)
      else offset
    in
    let operator_end = skip_whitespace_backward (end_offset - 1) in
    let rec contains_only_operator_characters offset =
      offset = operator_end
      || (Ocaml_operator.is_symbolic_character source_text.[offset]
          && contains_only_operator_characters (offset + 1))
    in
    if operator_start < operator_end && contains_only_operator_characters operator_start
    then
      { Range.start = position_of_offset source operator_start
      ; end_ = position_of_offset source operator_end
      }
    else range
;;

let prepare
      (state : State.t)
      { PrepareRenameParams.textDocument = { uri }; position; workDoneToken = _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let+ occurrences, (_ : Query_protocol.occurrences_status) =
      Document.Merlin.dispatch_exn
        ~name:"occurrences"
        merlin
        (Query_protocol.Occurrences (`Ident_at (Position.logical position), `Buffer))
    in
    let source = Document.source doc in
    List.find_map occurrences ~f:(fun (occurrence : Query_protocol.occurrence) ->
      if occurrence.is_stale
      then None
      else (
        let range = identifier_range source occurrence.loc in
        if Range.contains_position range position ~inclusive_end:true
        then Some range
        else None))
;;

let rename (state : State.t) { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position), `Renaming)
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
    let locs =
      List.fold_left
        locs
        ~init:(Map.empty (module Uri))
        ~f:(fun acc (loc : Warnings.loc) ->
          let uri =
            match loc.loc_start.pos_fname with
            | "" -> uri
            | path -> Uri.of_path path
          in
          Map.add_multi acc ~key:uri ~data:loc)
    in
    let edits =
      Map.mapi locs ~f:(fun ~key:doc_uri ~data:locs ->
        let source =
          match Document_store.get_opt state.store doc_uri with
          | Some doc when DocumentUri.equal doc_uri (Document.uri doc) ->
            Document.source doc
          | Some _ | None ->
            let source_path = Uri.to_path doc_uri in
            In_channel.with_open_text source_path In_channel.input_all |> Msource.make
        in
        List.map locs ~f:(fun loc ->
          let range = identifier_range source loc in
          let edit = TextEdit.create ~range ~newText:newName in
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
             | _ -> edit))
        |> List.stable_dedup ~compare:compare_text_edit)
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
          Map.to_alist edits
          |> List.map ~f:(fun (uri, edits) ->
            let version =
              Document_store.get_opt state.store uri |> Option.map ~f:Document.version
            in
            let textDocument =
              OptionalVersionedTextDocumentIdentifier.create ~uri ?version ()
            in
            let edits = List.map edits ~f:(fun e -> `TextEdit e) in
            `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits))
        in
        WorkspaceEdit.create ~documentChanges ())
      else (
        let changes = Map.to_alist edits in
        WorkspaceEdit.create ~changes ())
    in
    workspace_edits
;;
