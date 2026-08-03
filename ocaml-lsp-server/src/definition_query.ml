open Import
open Fiber.O

let location_of_merlin_loc uri : _ -> (_, string) result = function
  | `At_origin -> Error "Already at definition point"
  | `Builtin s ->
    Error (sprintf "%S is a builtin, it is not possible to jump to its definition" s)
  | `File_not_found s -> Error (sprintf "File_not_found: %s" s)
  | `Invalid_context -> Error "Not a valid identifier"
  | `Not_found (ident, where) ->
    let msg =
      let msg = sprintf "%S not found." ident in
      Option.value_map where ~default:msg ~f:(sprintf "%s last looked in %s" msg)
    in
    Error msg
  | `Not_in_env m -> Error (sprintf "Not in environment: %s" m)
  | `Found (path, lex_position) ->
    Ok
      (Position.of_lexical_position lex_position
       |> Option.map ~f:(fun position ->
         let range = { Range.start = position; end_ = position } in
         let uri = Option.value_map path ~default:uri ~f:Uri.of_path in
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
      let pos = Position.logical position in
      match kind with
      | `Definition -> Query_protocol.Locate (prefix, `ML, pos), "definition"
      | `Declaration -> Query_protocol.Locate (prefix, `MLI, pos), "declaration"
      | `Type_definition -> Query_protocol.Locate_type pos, "type definition"
    in
    let* result = Document.Merlin.dispatch_exn ~name doc command in
    (match location_of_merlin_loc uri result with
     | Ok s -> Fiber.return s
     | Error err_msg ->
       (* Merlin reports a failure for ordinary cursor positions, such as a
          keyword or a name that is already at its definition. Clients request
          definitions eagerly, so reporting these as request errors surfaces
          them to the user as spurious failures. *)
       Log.log ~section:"debug" (fun () ->
         Log.msg "locate failed" [ "kind", `String name; "error", `String err_msg ]);
       Fiber.return None)
;;
