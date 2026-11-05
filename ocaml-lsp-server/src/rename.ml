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
let identifier_range source (range : Range.t) =
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
        let range = Range.of_loc occurrence.loc |> identifier_range source in
        if Range.contains_position range position ~inclusive_end:true
        then Some range
        else None))
;;

(* In a qualified pun such as [{ M.x }], Merlin reports [M.x] as the variable
   occurrence but only [x] as the record-field occurrence. *)
type record_pun =
  { variable : Range.t
  ; field : Range.t
  ; end_ : Position.t (* Includes any type constraint or coercion. *)
  }

type file_occurrences =
  { source : Msource.t
  ; version : int option
  ; ranges : Range.t list
  ; record_puns : record_pun list
  ; has_field_occurrence : bool
  }

let record_puns_and_fields (parsetree : Mreader.parsetree) =
  let record_puns = ref [] in
  let record_fields = ref [] in
  let field_name_range (field : Longident.t Loc.loc) =
    let name = Longident.last field.txt in
    let loc = field.loc in
    let loc_start =
      { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - String.length name }
    in
    Range.of_loc { loc with loc_start }
  in
  let add_field (field : Longident.t Loc.loc) =
    record_fields := field_name_range field :: !record_fields
  in
  let add_pun (field : Longident.t Loc.loc) ~value_loc =
    let pun =
      { variable = Range.of_loc field.loc
      ; field = field_name_range field
      ; end_ = (Range.of_loc value_loc).end_
      }
    in
    record_puns := pun :: !record_puns;
    match field.txt with
    | Longident.Lident _ -> ()
    | _ -> record_fields := pun.field :: !record_fields
  in
  (* A constrained pun wraps its synthetic identifier. Compare the identifier's
     location, but keep the annotation in the range used to expand the pun. *)
  let rec expression_location (value : Parsetree.expression) =
    match value.pexp_desc with
    | Pexp_constraint (value, _) | Pexp_coerce (value, _, _) -> expression_location value
    | _ -> value.pexp_loc
  in
  let rec pattern_location (value : Parsetree.pattern) =
    match value.ppat_desc with
    | Ppat_constraint (value, _) -> pattern_location value
    | _ -> value.ppat_loc
  in
  let iterator =
    let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
      (match expr.pexp_desc with
       | Pexp_record (fields, _) ->
         List.iter fields ~f:(fun (field, value) ->
           if Loc.compare field.loc (expression_location value) = 0
           then add_pun field ~value_loc:value.pexp_loc
           else add_field field)
       | Pexp_field (_, field) | Pexp_setfield (_, field, _) -> add_field field
       | _ -> ());
      Ast_iterator.default_iterator.expr self expr
    in
    let pat (self : Ast_iterator.iterator) (pat : Parsetree.pattern) =
      (match pat.ppat_desc with
       | Ppat_record (fields, _) ->
         List.iter fields ~f:(fun (field, value) ->
           if Loc.compare field.loc (pattern_location value) = 0
           then add_pun field ~value_loc:value.ppat_loc
           else add_field field)
       | _ -> ());
      Ast_iterator.default_iterator.pat self pat
    in
    let label_declaration
          (self : Ast_iterator.iterator)
          (declaration : Parsetree.label_declaration)
      =
      record_fields := Range.of_loc declaration.pld_name.loc :: !record_fields;
      Ast_iterator.default_iterator.label_declaration self declaration
    in
    { Ast_iterator.default_iterator with expr; pat; label_declaration }
  in
  (match parsetree with
   | `Implementation structure -> iterator.structure iterator structure
   | `Interface signature -> iterator.signature iterator signature);
  !record_puns, !record_fields
;;

let same_range left right = Lsp.Range.compare left right = 0

let workspace_edit ~document_changes ~new_name files =
  let renames_record_field =
    List.exists files ~f:(fun (_, file) -> file.has_field_occurrence)
  in
  let edits =
    List.map files ~f:(fun (uri, file) ->
      let source = file.source in
      let edits =
        List.concat_map file.ranges ~f:(fun range ->
          let range = identifier_range source range in
          let pun =
            List.find file.record_puns ~f:(fun pun ->
              let pun_range = if renames_record_field then pun.field else pun.variable in
              same_range range pun_range)
          in
          match pun with
          | Some pun ->
            let variable =
              if not renames_record_field
              then new_name
              else (
                let (`Offset start_offset) =
                  Msource.get_offset source (Position.logical range.start)
                in
                let (`Offset end_offset) =
                  Msource.get_offset source (Position.logical range.end_)
                in
                String.sub
                  (Msource.text source)
                  ~pos:start_offset
                  ~len:(end_offset - start_offset))
            in
            (* Insert after any annotation, without replacing its source text. *)
            let expansion =
              TextEdit.create
                ~range:{ Range.start = pun.end_; end_ = pun.end_ }
                ~newText:(" = " ^ variable)
            in
            if renames_record_field
            then [ TextEdit.create ~range ~newText:new_name; expansion ]
            else [ expansion ]
          | None ->
            let edit = TextEdit.create ~range ~newText:new_name in
            let edit =
              match range.start with
              | { character = 0; _ } -> edit
              | pos ->
                let (`Offset index) =
                  let mpos = Position.logical pos in
                  Msource.get_offset source mpos
                in
                assert (index > 0)
                (* [index = 0] if we pass [`Logical (1, 0)], but we handle the case
                 when [character = 0] in a separate matching branch *);
                let source_txt = Msource.text source in
                (match source_txt.[index - 1] with
                 | '~' (* the occurrence is a named argument *)
                 | '?' (* is an optional argument *) ->
                   let empty_range_at_occur_end =
                     let occur_end_pos = edit.range.end_ in
                     { edit.range with start = occur_end_pos }
                   in
                   TextEdit.create
                     ~range:empty_range_at_occur_end
                     ~newText:(":" ^ new_name)
                 | _ -> edit)
            in
            [ edit ])
        |> List.stable_dedup ~compare:compare_text_edit
      in
      uri, file.version, edits)
  in
  if document_changes
  then (
    let documentChanges =
      List.map edits ~f:(fun (uri, version, edits) ->
        let textDocument =
          OptionalVersionedTextDocumentIdentifier.create ~uri ?version ()
        in
        let edits = List.map edits ~f:(fun e -> `TextEdit e) in
        `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits))
    in
    WorkspaceEdit.create ~documentChanges ())
  else (
    let changes = List.map edits ~f:(fun (uri, _, edits) -> uri, edits) in
    WorkspaceEdit.create ~changes ())
;;

let rename (state : State.t) { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let documents =
      Document_store.fold
        state.store
        ~init:(Map.empty (module Uri))
        ~f:(fun document documents ->
          Map.set documents ~key:(Document.uri document) ~data:document)
    in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position), `Renaming)
    in
    let* (occurrences, _desync), request_records =
      Document.Merlin.with_pipeline_exn ~name:"rename" merlin (fun pipeline ->
        let occurrences = Query_commands.dispatch pipeline command in
        let records = Mpipeline.reader_parsetree pipeline |> record_puns_and_fields in
        occurrences, records)
    in
    let locations =
      List.filter_map occurrences ~f:(fun (occurrence : Query_protocol.occurrence) ->
        match occurrence.is_stale with
        | true -> None
        | false ->
          let loc = occurrence.loc in
          let uri =
            match loc.loc_start.pos_fname with
            | "" -> uri
            | path -> Uri.of_path path
          in
          Some (uri, Range.of_loc loc))
    in
    (* Capture every source and version before yielding. Parsing and edits must
       use the same snapshots, including for documents other than the request. *)
    let files =
      List.fold_left
        locations
        ~init:(Map.empty (module Uri))
        ~f:(fun files (uri, range) -> Map.add_multi files ~key:uri ~data:range)
      |> Map.to_alist
      |> List.map ~f:(fun (uri, ranges) ->
        let document = Map.find documents uri in
        let source =
          match document with
          | Some document -> Document.source document
          | None ->
            In_channel.with_open_text (Uri.to_path uri) In_channel.input_all
            |> Msource.make
        in
        let version = Option.map document ~f:Document.version in
        uri, source, version, ranges)
    in
    let+ files =
      Fiber.parallel_map files ~f:(fun (file_uri, source, version, ranges) ->
        let+ record_puns, fields =
          (* The request's pipeline has already read its parsetree. Other files
             get separate pipelines; never nest compiler states on the worker. *)
          if Uri.equal uri file_uri
          then Fiber.return request_records
          else
            let* config =
              let handle = Merlin_config.DB.get state.merlin_config file_uri in
              Fiber.finalize
                (fun () -> Merlin_config.config handle)
                ~finally:(fun () -> Merlin_config.destroy handle)
            in
            let+ result =
              Document.Single_pipeline.use_with_config
                ~name:"rename-record-puns"
                state.merlin
                ~source
                ~config
                ~f:(fun pipeline ->
                  Mpipeline.reader_parsetree pipeline |> record_puns_and_fields)
            in
            match result with
            | Ok result -> result
            | Error exn -> Exn_with_backtrace.reraise exn
        in
        let has_field_occurrence =
          List.exists ranges ~f:(fun range -> List.exists fields ~f:(same_range range))
        in
        file_uri, { source; version; ranges; record_puns; has_field_occurrence })
    in
    let document_changes =
      Capabilities.workspace_edit_document_changes (State.client_capabilities state)
    in
    workspace_edit ~document_changes ~new_name:newName files
;;
