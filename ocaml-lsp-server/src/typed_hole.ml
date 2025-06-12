open Import
open Core
include Merlin_analysis.Typed_hole

let next_hole_cmd ~state ~(edit : TextEdit.t) =
  let supportsJumpToNextHole =
    State.experimental_client_capabilities state
    |> Client.Experimental_capabilities.supportsJumpToNextHole
  in
  if supportsJumpToNextHole
  then
    Some
      (Client.Custom_commands.next_hole
         ~in_range:(Range.resize_for_edit edit)
         ~notify_if_no_hole:false
         ())
  else None
;;

let next_hole ~(holes : Range.t list) ~(cursor : Position.t) =
  (* Find the first hole after the cursor, or default to the first *)
  let hole =
    List.find holes ~f:(fun hole ->
      match Position.compare hole.start cursor with
      | Lt | Eq -> false
      | Gt -> true)
  in
  match hole with
  | None -> List.hd holes
  | Some _ as hole -> hole
;;

let prev_hole ~(holes : Range.t list) ~(cursor : Position.t) =
  (* Find the last hole before the cursor, or default to the last *)
  let hole =
    List.fold_until
      holes
      ~init:None
      ~f:(fun prev_hole hole ->
        match Position.compare hole.end_ cursor with
        | Lt -> Continue (Some hole)
        | Gt | Eq -> Stop prev_hole)
      ~finish:Fn.id
  in
  match hole with
  | None -> List.last holes (* still only one scan, since we stopped on the first hole *)
  | Some _ as hole -> hole
;;
