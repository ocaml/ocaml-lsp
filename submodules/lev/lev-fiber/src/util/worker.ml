type 'a t = { work_chan : 'a Channel.t } [@@unboxed]
type task = Channel.elt_in_channel

let rec run t f =
  match Channel.get t.work_chan with
  | Ok v ->
      f v;
      run t f
  | Error `Closed -> ()

let create ~spawn_thread ~do_no_raise =
  let t = { work_chan = Channel.create () } in
  spawn_thread (fun () -> run t do_no_raise);
  t

let add_work t v =
  match Channel.send_removable t.work_chan v with
  | Error `Closed -> Error `Stopped
  | Ok _ as task -> task

let cancel_if_not_consumed = Channel.remove_if_not_consumed
let complete_tasks_and_stop t = Channel.close t.work_chan
