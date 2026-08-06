open Import
open Fiber.O
module Progress = Lsp_fiber.Progress

type t =
  | Disabled
  | Enabled of enabled

and enabled =
  { lifecycle : Progress.t
  ; mutable task : Progress.Task.t option
  }

let create (client_capabilities : ClientCapabilities.t) ~report_progress ~create_task =
  if Capabilities.work_done_progress client_capabilities
  then
    Enabled { lifecycle = Progress.create ~create_task ~report_progress (); task = None }
  else Disabled
;;

(* Dune runs a single build at a time, so we keep at most one task running:
   starting a new build ends the previous one first. *)
let end_task (t : enabled) ~message =
  let* () = Fiber.return () in
  match t.task with
  | None -> Fiber.return ()
  | Some task ->
    t.task <- None;
    Progress.end_ t.lifecycle task ~message
;;

let end_build_if_running = function
  | Disabled -> Fiber.return ()
  | Enabled t -> end_task t ~message:"Build interrupted"
;;

let start_build (t : enabled) =
  let* () = end_task t ~message:"Starting new build" in
  let+ task =
    Progress.start
      t.lifecycle
      ~token_name:"dune-build"
      ~title:"Build"
      ~message:"started"
      ()
  in
  t.task <- Some task;
  task
;;

let build_progress t (progress : Drpc.Progress.t) =
  Fiber.of_thunk (fun () ->
    match t with
    | Disabled -> Fiber.return ()
    | Enabled ({ lifecycle; _ } as t) ->
      (match progress with
       | Success -> end_task t ~message:"Build finished"
       | Failed -> end_task t ~message:"Build failed"
       | Interrupted -> end_task t ~message:"Build interrupted"
       | Waiting -> end_task t ~message:"Waiting for changes"
       | In_progress progress ->
         let* task =
           match t.task with
           | Some task -> Fiber.return task
           | None ->
             (* This can happen when we connect to dune in the middle of a
                build. *)
             let+ task = start_build t in
             task
         in
         let total = progress.complete + progress.remaining in
         (* The percentage is useless as it isn't monotinically increasing as
            the spec requires, but it's the best we can do. *)
         let percentage =
           if total = 0
           then 0
           else (
             let fraction = float_of_int progress.complete /. float_of_int total in
             int_of_float (fraction *. 100.))
         in
         Progress.report
           lifecycle
           task
           ~percentage
           ~message:(sprintf "Building [%d/%d]" progress.complete total)))
;;
