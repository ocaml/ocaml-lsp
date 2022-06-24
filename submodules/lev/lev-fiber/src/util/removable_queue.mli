type 'a t
(** Like a queue, but allows O(1) removal at any position *)

type 'a node

val data : 'a node -> 'a
val remove : _ node -> unit
val is_empty : _ t -> bool
val create : unit -> 'a t
val push : 'a t -> 'a -> 'a node
val pop : 'a t -> 'a option
val peek : 'a t -> 'a node option

val length : 'a t -> int
(** Numbers of elements in the queue. Runs in O(n). *)
