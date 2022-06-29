open Stdune

type 'a t = {
  q : 'a Removable_queue.t;
  m : Mutex.t;
  c : Condition.t;
  mutable is_closed : bool;
}

type elt_in_channel =
  | Node :
      Mutex.t (* mutex of the channel, where this element was sent *)
      * 'a Removable_queue.node
      -> elt_in_channel

let create () =
  {
    q = Removable_queue.create ();
    m = Mutex.create ();
    c = Condition.create ();
    is_closed = false;
  }

let with_mutex m ~f =
  Mutex.lock m;
  Exn.protect ~f:(fun () -> f ()) ~finally:(fun () -> Mutex.unlock m)

let is_empty t = with_mutex t.m ~f:(fun () -> Removable_queue.is_empty t.q)
let length t = with_mutex t.m ~f:(fun () -> Removable_queue.length t.q)

let send_removable t v =
  with_mutex t.m ~f:(fun () ->
      if t.is_closed then Error `Closed
      else
        let n = Removable_queue.push t.q v in
        Condition.signal t.c;
        Ok (Node (t.m, n)))

let send t v = send_removable t v |> Result.map ~f:ignore

let get t =
  with_mutex t.m ~f:(fun () ->
      let rec aux () =
        match Removable_queue.pop t.q with
        | Some v -> Ok v
        | None ->
            if t.is_closed then Error `Closed
            else (
              Condition.wait t.c t.m;
              aux ())
      in
      aux ())

let remove_if_not_consumed (Node (m, n)) =
  with_mutex m ~f:(fun () -> Removable_queue.remove n)

let close t =
  with_mutex t.m ~f:(fun () ->
      if not t.is_closed then (
        t.is_closed <- true;
        Condition.signal t.c))
