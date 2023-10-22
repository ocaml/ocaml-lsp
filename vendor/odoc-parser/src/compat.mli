(** @since 4.08 *)
module Option : sig
  type 'a t = 'a option = None | Some of 'a

  val is_some : 'a option -> bool
  (** [is_some o] is [true] if and only if [o] is [Some o]. *)

  val value : default:'a -> 'a option -> 'a
  val join_list : 'a option list -> 'a list option
end

module Char : sig
  include module type of Char

  val equal : t -> t -> bool
  (** The equal function for chars.
      @since 4.03.0 *)
end

module String : sig
  include module type of String

  val for_all : (char -> bool) -> string -> bool
  (** [for_all p s] checks if all characters in [s] satisfy the preficate [p].
      @since 4.13.0 *)
end
