open Import

type 'a t =
  { work_chan : 'a Channel.t
  ; mutable th : Thread.t option
  }

type task = Channel.elt_in_channel

let run (f, t) =
  let rec loop () =
    match Channel.get t.work_chan with
    | Ok v ->
      f v;
      loop ()
    | Error `Closed -> ()
  in
  loop ()

let create ~do_ =
  let t = { work_chan = Channel.create (); th = None } in
  let th = Thread.create run (do_, t) in
  t.th <- Some th;
  t

let add_work t v =
  match Channel.send_removable t.work_chan v with
  | Error `Closed -> Error `Stopped
  | Ok _ as task -> task

let cancel = Channel.remove_if_not_consumed

let complete_tasks_and_stop t =
  match Channel.close t.work_chan with
  | Ok () -> ()
  | Error `Already_closed ->
    (* failwith "channel already closed" *)
    ()
