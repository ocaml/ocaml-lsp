type 'a t

(** [make arr ~pos ~len ()] can be thought of a new array for which the 0-th
    element is [arr.(pos)] and has length [len] if specified. If [len] is
    omitted, [Array.length arr - pos] is taken as the length. Importantly, the
    "new array" does not copy but simply references [arr]. Hence, creating views
    is constant time. However, keep in mind that since a view references an
    array, the array will be alive in memory as long as the view is alive.

    @raise Invalid_argument
      if [pos + len > Array.length arr] or [pos < 0 || pos >= Array.length arr]*)
val make : 'a array -> pos:int -> ?len:int -> unit -> 'a t

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val is_empty : 'a t -> bool

val length : 'a t -> int

(** compute length of common suffix between two sub-arrays *)
val common_suffix_len : 'a t -> 'a t -> int
