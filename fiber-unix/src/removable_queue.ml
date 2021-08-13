(* Implementation of a doubly linked list, based on Cormen et al. "Introduction
   to Algorithms" and
   https://github.com/janestreet/jenga/blob/114.04/tenacious/lib/ring.ml for
   hacky OCaml-specific pieces. *)

type 'a t =
  { data : 'a option
  ; mutable prev : 'a t
        (* Invariant: if [node.prev == node] then [node] has been removed from
           the list. Easy to hold invariant that frees us from having a separate
           mutable record field to hold this information. *)
  ; mutable next : 'a t
        (* Invariant: if [node.next == node] then it's the sentinel *)
  }

type 'a node = 'a t

let create () : 'a t =
  let rec sentinel =
    { data =
        None
        (* sentinel doesn't hold [data]; so this field should never be
           accessed *)
    ; prev = sentinel
    ; next = sentinel
    }
  in
  sentinel

let is_empty (sentinel : 'a t) : bool = sentinel.next == sentinel

let length (sentinel : 'a t) : int =
  let head = ref sentinel.next in
  let count = ref 0 in
  while not (!head == sentinel) do
    incr count;
    head := !head.next
  done;
  !count

(* appends a tail node *)
let push : 'a. 'a t -> 'a -> 'a node =
 fun sentinel v ->
  let inserted_node =
    { data = Some v; prev = sentinel.prev; next = sentinel }
  in
  sentinel.prev.next <- inserted_node;
  sentinel.prev <- inserted_node;
  inserted_node

let mark_as_detached (node : 'a node) = node.prev <- node

let is_detached (node : 'a node) = node.prev == node

(* removes head node *)
let pop : 'a. 'a t -> 'a option =
 fun sentinel ->
  if is_empty sentinel then
    None
  else
    let removed_node = sentinel.next in
    sentinel.next <- removed_node.next;
    removed_node.next.prev <- sentinel;
    mark_as_detached removed_node;
    removed_node.data

let remove node : unit =
  if not (is_detached node) then (
    node.prev.next <- node.next;
    node.next.prev <- node.prev;
    mark_as_detached node
  )
