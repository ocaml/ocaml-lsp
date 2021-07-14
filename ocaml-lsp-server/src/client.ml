open Import

module Vscode = struct
  module Commands = struct
    let triggerSuggest =
      Command.create ~title:"Trigger Suggest"
        ~command:"editor.action.triggerSuggest" ()

    module Custom = struct
      let next_hole ?start_position ~notify_if_no_hole () =
        let arguments =
          let arg_obj_fields =
            let notif_json =
              Some ("notify-if-no-hole", Json.bool notify_if_no_hole)
            in
            let pos_json =
              Option.map start_position ~f:(fun p ->
                  ("position", Position.yojson_of_t p))
            in
            List.filter_opt [ pos_json; notif_json ]
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
