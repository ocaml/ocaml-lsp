(** Simon Cooke's Bip_buffer *)

module Blit : sig
  type ('src, 'dst) t =
    src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit
end

module Slice : sig
  type t = { pos : int; len : int }
end

type 'a t
(** a bip buffer with some underlying container of bytes indexed by a continuous range
    of integers that starts from 0. *)

val max_available : _ t -> int
(** [max_available t] returns the maximum available contiguous write size the
    buffer can accept *)

val best_available : _ t -> int
(** [best_available t] returns the best available contiguous write size the
    buffer can accept. If all writes are smaller than [best_available t], it is
    guaranteed that no space will be wasted. *)

val is_empty : _ t -> bool
(** [is_empty t] true if there are no bytes available to read in [t] *)

val length : _ t -> int
(** [length t] returns the number of bytes readable in [t] *)

val buffer : 'a t -> 'a
(** [buffer t] returns the underlying buffer of [t] for reading/writing *)

val create : 'a -> len:int -> 'a t
(** [create buf ~len] creates a new buffer with underlying storage [buf] of length [len] *)

val junk : _ t -> len:int -> unit
(** [junk t ~len] discards [len] from the front of the buffer. Usually called after reading *)

val peek : _ t -> Slice.t option
(** [peek t] returns the next contiguous readable buffer slice as [Some _]. If
    [t] is empty, [None] is returned. Once a portion of this slice is read,
    [junk] should be called. *)

val reserve : _ t -> len:int -> int option
val commit : _ t -> len:int -> unit

val unused_space : _ t -> int
(** Total amount of free space available in the buffer. Not all of it may be
    usable. To reclaim it, call [compress] *)

val compress : 'a t -> ('a, 'a) Blit.t -> unit
(** [compress t blit] will try to compress the buffer with 2 blit operations.
    Use [unused_space t] to asses how useful this will be. *)

val resize : 'a t -> ('a, 'a) Blit.t -> 'a -> len:int -> unit
(** [resize t blit buf ~len] will create a new buffer with the same data. The
    old buffer is then emptied and can be reused *)

val pp :
  (Format.formatter -> 'a * Slice.t -> unit) -> Format.formatter -> 'a t -> unit

module Bytes : sig
  type nonrec t = Bytes.t t

  val resize : t -> len:int -> unit
  val compress : t -> unit
  val pp : Format.formatter -> t -> unit

  module Writer : sig
    (** This module will automatically resize/compress the buffer when space
        runs out. If you want to make sure the buffer doesn't grow, make sure
        the writes are within [max_available] or [best_available]. *)

    val add_char : t -> char -> unit
    val add_string : t -> string -> unit
    val add_substring : t -> string -> pos:int -> len:int -> unit
    val add_bytes : t -> Bytes.t -> unit
    val add_subbytes : t -> Bytes.t -> pos:int -> len:int -> unit
  end
end
