open Import

let action_kind = "annotate"

let check_context doc pos_start =
  Document.with_pipeline doc (fun pipeline ->
      let pos_start = Mpipeline.get_lexing_pos pipeline pos_start in
      let typer = Mpipeline.typer_result pipeline in
      let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
      match Mbrowse.enclosing pos_start [ browse ] with
      | (_, (Expression _ | Pattern _)) :: _ -> `Valid
      | _ :: _
      | [] ->
        `Invalid)

let get_source_text doc (loc : Loc.t) =
  let open Option.O in
  let source = Document.source doc in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  let (`Offset start) = Msource.get_offset source (Position.logical start) in
  let (`Offset end_) = Msource.get_offset source (Position.logical end_) in
  String.sub (Msource.text source) ~pos:start ~len:(end_ - start)

let code_action_of_type_enclosing uri doc (loc, typ) =
  let open Option.O in
  let+ original_text = get_source_text doc loc in
  let newText = Printf.sprintf "(%s : %s)" original_text typ in
  let edit : WorkspaceEdit.t =
    let textedit : TextEdit.t = { range = Range.of_loc loc; newText } in
    let uri = Uri.to_string uri in
    WorkspaceEdit.create ~changes:[ (uri, [ textedit ]) ] ()
  in
  let title = String.capitalize_ascii action_kind in
  CodeAction.create ~title ~kind:(CodeActionKind.Other action_kind) ~edit
    ~isPreferred:false ()

let code_action doc (params : CodeActionParams.t) =
  let open Fiber.O in
  let uri = Uri.t_of_yojson (`String params.textDocument.uri) in
  let pos_start = Position.logical params.range.start in
  let* context = check_context doc pos_start in
  match context with
  | Error e -> Fiber.return (Error (Jsonrpc.Response.Error.of_exn e))
  | Ok `Invalid -> Fiber.return (Ok None)
  | Ok `Valid -> (
    let command = Query_protocol.Type_enclosing (None, pos_start, None) in
    let+ res =
      Document.with_pipeline doc (fun pipeline ->
          let config = Mpipeline.final_config pipeline in
          let config =
            { config with query = { config.query with verbosity = 0 } }
          in
          let pipeline = Mpipeline.make config (Document.source doc) in
          Query_commands.dispatch pipeline command)
    in
    match res with
    | Ok []
    | Ok ((_, `Index _, _) :: _) ->
      Ok None
    | Ok ((location, `String value, _) :: _) ->
      Ok (code_action_of_type_enclosing uri doc (location, value))
    | Error e -> Error (Jsonrpc.Response.Error.of_exn e))
