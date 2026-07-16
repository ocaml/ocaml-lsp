(** Code action to wrap a type in a module. Useful for not having to write boilerplate
    module ... = struct ... end every time you define a type. Example:
    {[
      type 'a foo =
        { a : int
        ; b : int
        ; c : 'a list
        }
      [@@deriving sexp]
    ]}
    becomes
    {[
      module Foo = struct
        type 'a t =
          { a : int
          ; b : int
          ; c : 'a list
          }
        [@@deriving sexp]
      end
    ]}
    after the action. Types already named "t" are ignored. *)

val t : Code_action.t
