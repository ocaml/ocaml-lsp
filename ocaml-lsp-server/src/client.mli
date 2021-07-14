open Import

(** This module is a collection of client-specific functionality (client =
    editor) *)

module Vscode : sig
  (** A collection of VS Code editor commands.

      Reference for VS Code built-in commands:
      https://code.visualstudio.com/api/references/commands *)
  module Commands : sig
    (** [editor.action.triggerSuggest] is a vscode-specific command, which
        triggers the completion request on all completion providers *)
    val triggerSuggest : Command.t

    (** Represents custom commands, i.e., commands added by a certain extension.

        Currently, the module includes custom commands introduced by "OCaml
        Platform" extension *)
    module Custom : sig
      (** Request client cursor to jump to the next hole.

          Looks for a hole starting at position [start_position], if provided;
          otherwise, uses the cursor position.

          Will not show a pop-up notification if [notify-if-no-hole] is set to
          [false] (the default value is [true]) *)
      val next_hole :
           ?start_position:Position.t
        -> notify_if_no_hole:bool
        -> unit
        -> Command.t
    end
  end
end
