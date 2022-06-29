module Id = Int

type 'a t = { work_chan : 'a Channel.t; thread : Thread.t }
type task = Channel.elt_in_channel

let id t = Thread.id t.thread
let join t = Thread.join t.thread

let rec run work_chan f =
  match Channel.get work_chan with
  | Error `Closed -> ()
  | Ok v ->
      f v;
      run work_chan f

let create ~spawn_thread ~do_no_raise =
  let work_chan = Channel.create () in
  let thread = spawn_thread (fun () -> run work_chan do_no_raise) in
  { work_chan; thread }

let add_work t v =
  match Channel.send_removable t.work_chan v with
  | Error `Closed -> Error `Stopped
  | Ok _ as task -> task

let cancel_if_not_consumed = Channel.remove_if_not_consumed
let complete_tasks_and_stop t = Channel.close t.work_chan
