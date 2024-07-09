val infer_intf_for_impl : Document.t -> string Fiber.t

(** Called by the code action "insert inferred interface". Gets the Merlin
    typer_result for both the implementation and interface documents, and uses
    the diff between them to produce the updated interface. Any names present in
    the existing interface are omitted from the inserted code (regardless of
    whether their signatures have changed). *)
val infer_missing_intf_for_impl
  :  Document.t (** implementation *)
  -> Document.t (** interface *)
  -> string Fiber.t
(** code to be inserted in the interface *)

val infer_intf : State.t -> Document.t -> string option Fiber.t

(** Called by the code action "update signature(s) to match implementation".
    Compares signatures found in the selected range of the interface document
    with ones inferred from the corresponding implementation document, and
    produces text edits for any that can be updated. *)
val update_signatures
  :  state:State.t
  -> intf_merlin:Document.Merlin.t
  -> doc:Document.t
  -> range:Range.t
  -> Import.TextEdit.t list Fiber.t
