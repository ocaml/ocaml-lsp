open Import

(** This module is a collection of client-specific functionality (client =
    editor) *)

module Experimental_capabilities : sig
  (** Module to store experimental client capabilities *)

  type t

  val of_opt_json : Json.t option -> t
  val supportsJumpToNextHole : t -> bool
end

module Vscode : sig
  (** A collection of VS Code editor commands.

      Reference for VS Code built-in commands:
      https://code.visualstudio.com/api/references/commands *)
  module Commands : sig
    (** [editor.action.triggerSuggest] is a vscode-specific command, which
        triggers the completion request on all completion providers *)
    val triggerSuggest : Command.t
  end
end

(** Represents custom commands, i.e., commands added by a certain extension. *)
module Custom_commands : sig
  (** Request client cursor to jump to the next hole.

      See the documentation for this command in [vscode-ocaml-platform] for
      details.

      @param in_range
        to pick a hole only in a given range; if omitted, the whole document is
        used
      @param notify_if_no_hole
        specifies whether we want the client to show the user a message if there
        is no hole to jump to *)
  val next_hole : ?in_range:Range.t -> notify_if_no_hole:bool -> unit -> Command.t
end
