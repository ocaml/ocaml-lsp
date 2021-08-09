(** Implementation of a doubly linked list, based on Cormen et al. "Introduction
    to Algorithms", using a self-referencing sentinel node. *)

(** Type that represents a doubly linked list *)
type 'a t

(** Creates a new doubly linked list *)
val create : unit -> 'a t

(** Checks whether a doubly linked list is empty *)
val is_empty : 'a t -> bool

(** Returns the number of elements in the list *)
val length : 'a t -> int

(** Type that represents a node in a doubly linked list. It can be used to
    remove the node from the list. *)
type 'a node

(** [prepend lst v] adds a new node containing [v] as the head of the list *)
val prepend : 'a t -> 'a -> 'a node

(** Removes the first element in the list *)
val detach_head : 'a t -> 'a option

(** [append lst v] adds a new node containing [v] to the end of the list *)
val append : 'a t -> 'a -> 'a node

(** Removes the last element in the list *)
val detach_tail : 'a t -> 'a option

val detach : 'a node -> (unit, [ `Already_detached ]) result
