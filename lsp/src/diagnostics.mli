open! Import

module Tag : sig
  type t =
    | Unnecessary
    | Deprecated

  include Json.Jsonable.S with type t := t
end
