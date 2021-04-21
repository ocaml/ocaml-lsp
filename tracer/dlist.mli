(** Doubly linked list with all operations constant time *)

type 'a t

val create : unit -> 'a t

val length : 'a t -> int

val prepend : 'a t -> 'a -> unit

val append : 'a t -> 'a -> unit

(** @raise Invalid_argument if the list is empty *)
val pop_right_exn : 'a t -> 'a

(** drops an element from right side of the list

    @raise Invalid_argument if the list is empty *)
val drop_one_right_exn : 'a t -> unit

val to_list : 'a t -> 'a list
