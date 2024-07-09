open Import
open Fiber.O

let location_of_merlin_loc uri : _ -> (_, string) result = function
  | `At_origin -> Ok None
  | `Builtin _ -> Ok None
  | `File_not_found s -> Error (sprintf "File_not_found: %s" s)
  | `Invalid_context -> Ok None
  | `Not_found (ident, where) ->
    let msg =
      let msg = sprintf "%s not found." ident in
      match where with
      | None -> msg
      | Some w -> sprintf "%s last looked in %s" msg w
    in
    Error msg
  | `Not_in_env m -> Error (sprintf "not in environment: %s" m)
  | `Found (path, lex_position) ->
    Ok
      (Position.of_lexical_position lex_position
       |> Option.map ~f:(fun position ->
         let range = { Range.start = position; end_ = position } in
         let uri =
           match path with
           | None -> uri
           | Some path -> Uri.of_path path
         in
         let locs = [ { Location.uri; range } ] in
         `Location locs))
;;

let run kind (state : State.t) uri position =
  let* () = Fiber.return () in
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    let command, name =
      let pos = Position.logical position in
      match kind with
      | `Definition -> Query_protocol.Locate (None, `ML, pos), "definition"
      | `Declaration -> Query_protocol.Locate (None, `MLI, pos), "declaration"
      | `Type_definition -> Query_protocol.Locate_type pos, "type definition"
    in
    let* result = Document.Merlin.dispatch_exn ~name doc command in
    (match location_of_merlin_loc uri result with
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
            ~data:
              (`String (sprintf "'Locate' query to merlin returned error: %s" err_msg))
            ()))
;;
