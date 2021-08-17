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

(** [send_removable_many t lst] is same as [List.map (send_removable t) lst] but
    doesn't reacquire lock for every single element in the given list *)
val send_removable_many :
  'a t -> 'a list -> (elt_in_channel list, [ `Closed ]) result

(** similar to [send_removable] but the element is non-removable from the
    channel once sent *)
val send : 'a t -> 'a -> (unit, [ `Closed ]) result

(** similar to [send_removable_many] but the elements in the channel are
    non-removable once sent *)
val send_many : 'a t -> 'a list -> (unit, [ `Closed ]) result

(** remove element put in the channel if it hasn't been consumed yet *)
val remove_if_not_consumed : elt_in_channel -> unit

(** [get ch] reads a value [v] from the channel [ch]. If there is no value to
    read, the thread sleeps until there is some value to read. *)
val get : 'a t -> ('a, [ `Closed ]) result

val close : 'a t -> unit
