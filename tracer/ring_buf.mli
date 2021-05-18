(** A ring buffer backed by an array *)

type 'a t

val create : size:int -> 'a t

(** [push_back buf elt] pushes [elt] to the back of [buf]. If the [buf] is full,
    the oldest element is replaced first *)
val push_back : 'a t -> 'a -> unit

(** returns the ring buffer as a list from the oldest to the youngest element in
    the buffer *)
val to_list : 'a t -> 'a list
