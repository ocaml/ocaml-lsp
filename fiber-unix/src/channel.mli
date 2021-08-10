type 'a t

type elt_in_channel

val create : unit -> 'a t

(** Checks whether the channel contains elements *)
val is_empty : 'a t -> bool

(** Number of elements currently in the channel not consumed. Runs in O(n). *)
val length : 'a t -> int

(** [send_removable ch v] puts a value [v] in the channel [ch] in a non-blocking
    manner (we consider acquiring/waiting for the lock to be "non-blocking").
    The returned value [elt_in_channel] can then be used to remove the put
    element from the channel if it hasn't been consumed by the other end of the
    channel yet. *)
val send_removable : 'a t -> 'a -> (elt_in_channel, [ `Closed ]) result

(** similar to [put_removable] but the returned value is ignored *)
val send : 'a t -> 'a -> (unit, [ `Closed ]) result

(** remove element put in the channel if it hasn't been consumed yet *)
val remove_if_not_consumed : elt_in_channel -> unit

(** [get ch] reads a value [v] from the channel [ch]. If there is no value to
    read, the thread sleeps until there is some value to read. *)
val get : 'a t -> ('a, [ `Closed ]) result

val close : 'a t -> (unit, [ `Already_closed ]) result
