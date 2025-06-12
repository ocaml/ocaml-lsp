open Import
open Fiber.O

let make_construct_action ~state ~doc loc newText =
  let title = [%string "Construct: %{newText}"] in
  let kind = CodeActionKind.Other "construct" in
  let range : Range.t = Range.of_loc loc in
  let edit = Document.edit doc [ { range; newText } ] in
  let command = Typed_hole.next_hole_cmd ~state ~edit:{ range; newText } in
  CodeAction.create ~title ~kind ~edit ?command ~isPreferred:false ()
;;

(* Called by compute_ocaml_code_actions to get all construct options. *)
let get_construct_actions ~log_info (state : State.t) doc (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return []
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return []
  | `Merlin merlin ->
    let pos = Position.logical params.range.Range.end_ in
    let src = Document.source doc in
    (* First do a fast local check to rule out non-holes. *)
    let prefix = Compl.prefix_of_position ~short_path:false src pos in
    let suffix = Compl.suffix_of_position src pos in
    (match Typed_hole.can_be_hole prefix || Typed_hole.can_be_hole suffix with
     | false -> Fiber.return []
     | true ->
       (* Then use the Merlin type information for a slower check. *)
       let* structures =
         Document.Merlin.with_pipeline_exn ~log_info merlin (fun pipeline ->
           let typer = Mpipeline.typer_result pipeline in
           let typedtree = Mtyper.get_typedtree typer in
           let pos = Mpipeline.get_lexing_pos pipeline pos in
           Mbrowse.enclosing pos [ Mbrowse.of_typedtree typedtree ])
       in
       (match Typed_hole.is_a_hole structures with
        | false -> Fiber.return []
        | true ->
          (* Finally ask Merlin for the actual constructions. *)
          let command = Query_protocol.Construct (pos, Some `Local, Some 1) in
          let+ res = Document.Merlin.dispatch ~log_info merlin command in
          (match res with
           | Ok (loc, constructions) ->
             Core.List.map constructions ~f:(make_construct_action ~state ~doc loc)
           | Error _ -> [])))
;;
