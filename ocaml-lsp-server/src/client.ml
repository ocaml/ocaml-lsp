open Import

module Experimental_capabilities = struct
  type t = bool

  let of_opt_json (json : Json.t option) =
    match json with
    | Some (`Assoc fields) ->
      Json.field fields "jumpToNextHole" Json.Conv.bool_of_yojson
      |> Option.value ~default:false
    | _ -> false
  ;;

  let supportsJumpToNextHole t = t
end

module Vscode = struct
  module Commands = struct
    let triggerSuggest =
      Command.create ~title:"Trigger Suggest" ~command:"editor.action.triggerSuggest" ()
    ;;
  end
end

module Custom_commands = struct
  let next_hole ?in_range ~notify_if_no_hole () =
    let arguments =
      let arg_obj_fields =
        let notif_json = Some ("shouldNotifyIfNoHole", Json.bool notify_if_no_hole) in
        let in_range_json =
          Option.map in_range ~f:(fun r -> "inRange", Range.yojson_of_t r)
        in
        List.filter_opt [ in_range_json; notif_json ]
      in
      match arg_obj_fields with
      | [] -> [] (* no arguments -- the extension uses defaults *)
      | fields ->
        (* the use of a (json) object as the first and single argument to the
           command is intended *)
        [ `Assoc fields ]
    in
    Command.create ~title:"Jump to Next Hole" ~command:"ocaml.next-hole" ~arguments ()
  ;;
end
