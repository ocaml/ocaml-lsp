open Import

type 'a t =
  { q : 'a Removable_queue.t
  ; m : Mutex.t
  ; c : Condition.t
  ; mutable is_closed : bool
  }

type elt_in_channel =
  | Node :
      'a t (* to be able to lock the mutex *) * 'a Removable_queue.node
      -> elt_in_channel

let create () =
  { q = Removable_queue.create ()
  ; m = Mutex.create ()
  ; c = Condition.create ()
  ; is_closed = false
  }

let is_empty t = with_mutex t.m ~f:(fun () -> Removable_queue.is_empty t.q)

let length t = with_mutex t.m ~f:(fun () -> Removable_queue.length t.q)

let send_removable t v =
  with_mutex t.m ~f:(fun () ->
      if t.is_closed then
        Error `Closed
      else
        let n = Removable_queue.push t.q v in
        Condition.signal t.c;
        Ok (Node (t, n)))

let send t v =
  with_mutex t.m ~f:(fun () ->
      if t.is_closed then
        Error `Closed
      else
        let (_ : 'a Removable_queue.node) = Removable_queue.push t.q v in
        Condition.signal t.c;
        Ok ())

let get t =
  with_mutex t.m ~f:(fun () ->
      let rec aux () =
        match Removable_queue.pop t.q with
        | Some v -> Ok v
        | None ->
          if t.is_closed then
            Error `Closed
          else (
            Condition.wait t.c t.m;
            aux ()
          )
      in
      aux ())

let remove_if_not_consumed (Node (t, n)) =
  with_mutex t.m ~f:(fun () -> Removable_queue.remove n)

let close t =
  with_mutex t.m ~f:(fun () ->
      if t.is_closed then
        Error `Already_closed
      else (
        t.is_closed <- true;
        Ok ()
      ))
