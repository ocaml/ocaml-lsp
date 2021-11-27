open Import

module Vscode = struct
  module Commands = struct
    let triggerSuggest =
      Command.create ~title:"Trigger Suggest"
        ~command:"editor.action.triggerSuggest" ()

    module Custom = struct
      let next_hole ?in_range ~notify_if_no_hole () =
        let arguments =
          let arg_obj_fields =
            let notif_json =
              Some ("shouldNotifyIfNoHole", Json.bool notify_if_no_hole)
            in
            let in_range_json =
              Option.map in_range ~f:(fun r -> ("inRange", Range.yojson_of_t r))
            in
            List.filter_opt [ in_range_json; notif_json ]
          in
          match arg_obj_fields with
          | [] -> [] (* no arguments -- the extension uses defaults *)
          | fields ->
            (* the use of a (json) object as the first and single argument to
               the command is intended *)
            [ `Assoc fields ]
        in
        Command.create ~title:"Jump to Next Hole" ~command:"ocaml.next-hole"
          ~arguments ()
    end
  end
end
