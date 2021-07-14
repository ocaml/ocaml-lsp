open Import

module Vscode = struct
  module Commands = struct
    let triggerSuggest =
      Command.create ~title:"Trigger Suggest"
        ~command:"editor.action.triggerSuggest" ()

    module Custom : sig
      val next_hole :
           ?start_position:Position.t
        -> ?notify_if_no_hole:bool
        -> unit
        -> Command.t
    end = struct
      let next_hole ?start_position ?notify_if_no_hole () =
        let arguments =
          let pos_json =
            Option.map start_position ~f:(fun p ->
                ("position", Position.yojson_of_t p))
          in
          let notif_json =
            Option.map notify_if_no_hole ~f:(fun b ->
                ("notify-if-no-hole", Json.bool b))
          in
          let arg_obj_fields =
            List.filter_map [ pos_json; notif_json ] ~f:Fun.id
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
