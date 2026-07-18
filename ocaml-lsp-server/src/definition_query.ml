open Import
open Fiber.O

let location_of_merlin_loc state doc uri : _ -> (_, string) result = function
  | `At_origin -> Error "Already at definition point"
  | `Builtin s ->
    Error (sprintf "%S is a builtin, it is not possible to jump to its definition" s)
  | `File_not_found s -> Error (sprintf "File_not_found: %s" s)
  | `Invalid_context -> Error "Not a valid identifier"
  | `Not_found (ident, where) ->
    let msg =
      let msg = sprintf "%S not found." ident in
      match where with
      | None -> msg
      | Some w -> sprintf "%s last looked in %s" msg w
    in
    Error msg
  | `Not_in_env m -> Error (sprintf "Not in environment: %s" m)
  | `Found (path, lex_position) ->
    let uri, target_doc =
      match path with
      | None -> uri, Some doc
      | Some path ->
        let uri = Uri.of_path path in
        uri, Document_store.get_opt state.State.store uri
    in
    let position =
      match target_doc, path with
      | Some doc, _ -> Document.position_of_lexical_position doc lex_position
      | None, Some path ->
        (match
           Exn_with_backtrace.try_with (fun () ->
             In_channel.with_open_text path In_channel.input_all |> Msource.make)
         with
         | Ok source ->
           Document.position_of_lexical_position_in_source doc source lex_position
         | Error _ -> Position.of_lexical_position lex_position)
      | None, None -> Document.position_of_lexical_position doc lex_position
    in
    Ok
      (Option.map position ~f:(fun position ->
         let range = { Range.start = position; end_ = position } in
         let locs = [ { Location.uri; range } ] in
         `Location locs))
;;

let run kind (state : State.t) ?prefix uri position =
  let* () = Fiber.return () in
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    let command, name =
      let pos =
        (Document.merlin_position (Document.Merlin.to_doc doc) position
          :> Msource.position)
      in
      match kind with
      | `Definition -> Query_protocol.Locate (prefix, `ML, pos), "definition"
      | `Declaration -> Query_protocol.Locate (prefix, `MLI, pos), "declaration"
      | `Type_definition -> Query_protocol.Locate_type pos, "type definition"
    in
    let* result = Document.Merlin.dispatch_exn ~name doc command in
    (match location_of_merlin_loc state (Document.Merlin.to_doc doc) uri result with
     | Ok s -> Fiber.return s
     | Error err_msg ->
       let kind =
         match kind with
         | `Definition -> "definition"
         | `Declaration -> "declaration"
         | `Type_definition -> "type definition"
       in
       Jsonrpc.Response.Error.raise
         (Jsonrpc.Response.Error.make
            ~code:Jsonrpc.Response.Error.Code.RequestFailed
            ~message:(sprintf "Request \"Jump to %s\" failed." kind)
            ~data:(`String (sprintf "Locate: %s" err_msg))
            ()))
;;
