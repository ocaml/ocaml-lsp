open Import

module Vscode = struct
  (** A collection of VS Code editor commands.

      Reference for VS Code built-in commands:
      https://code.visualstudio.com/api/references/commands *)
  module Commands = struct
    (** [editor.action.triggerSuggest] is a vscode-specific command, which
        triggers the completion request on all completion providers *)
    let triggerSuggest =
      Command.create ~title:"Trigger Suggest"
        ~command:"editor.action.triggerSuggest" ()

    (** Represents custom commands, i.e., commands added by an extension.

        Currently, the module includes custom commands introduced by "Ocaml
        Platform" extension *)
    module Custom : sig
      (** Request client cursor to jump to the next hole. Looks for a hole
          starting at position [start_position], if provided; otherwise, uses
          the cursor position. Will not show a pop-up notification if
          [notify-if-no-hole] is set to [false] (the default value is [true]) *)
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
