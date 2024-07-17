open Import
open Fiber.O
module Progress = Lsp.Progress

type enabled =
  { (* TODO this needs to be mutexed *)
    mutable token : ProgressToken.t option
  ; mutable build_counter : int
  ; report_progress : Progress.t ProgressParams.t -> unit Fiber.t
  ; create_task : WorkDoneProgressCreateParams.t -> unit Fiber.t
  }

type t =
  | Disabled
  | Enabled of enabled

let create (client_capabilities : ClientCapabilities.t) ~report_progress ~create_task =
  match client_capabilities.window with
  | Some { workDoneProgress = Some true; _ } ->
    Enabled { token = None; build_counter = 0; create_task; report_progress }
  | _ -> Disabled
;;

let end_build (t : enabled) ~message =
  Fiber.of_thunk (fun () ->
    match t.token with
    | None -> Fiber.return ()
    | Some token ->
      t.token <- None;
      t.report_progress
        (ProgressParams.create
           ~token
           ~value:(Progress.End (WorkDoneProgressEnd.create ~message ()))))
;;

let end_build_if_running = function
  | Disabled -> Fiber.return ()
  | Enabled e -> end_build e ~message:"Build interrupted"
;;

let start_build (t : enabled) =
  let* () = end_build t ~message:"Starting new build" in
  let token = `String ("dune-build-" ^ Int.to_string t.build_counter) in
  t.token <- Some token;
  t.build_counter <- t.build_counter + 1;
  let* () = t.create_task (WorkDoneProgressCreateParams.create ~token) in
  t.token <- Some token;
  let+ () =
    t.report_progress
      (ProgressParams.create
         ~token
         ~value:
           (Progress.Begin
              (WorkDoneProgressBegin.create ~title:"Build" ~message:"started" ())))
  in
  token
;;

let build_progress t (progress : Drpc.Progress.t) =
  Fiber.of_thunk (fun () ->
    match t with
    | Disabled -> Code_error.raise "progress reporting is not supported" []
    | Enabled ({ token; report_progress; _ } as t) ->
      (match progress with
       | Success -> end_build t ~message:"Build finished"
       | Failed -> end_build t ~message:"Build failed"
       | Interrupted -> end_build t ~message:"Build interrupted"
       | Waiting -> end_build t ~message:"Waiting for changes"
       | In_progress progress ->
         let* token =
           match token with
           | Some token -> Fiber.return token
           | None ->
             (* This can happen when we connect to dune in the middle of a
                build. *)
             start_build t
         in
         let total = progress.complete + progress.remaining in
         (* The percentage is useless as it isn't monotinically increasing as
            the spec requires, but it's the best we can do. *)
         let percentage =
           let fraction = float_of_int progress.complete /. float_of_int total in
           int_of_float (fraction *. 100.)
         in
         report_progress
           (ProgressParams.create
              ~token
              ~value:
                (Progress.Report
                   (let message = sprintf "Building [%d/%d]" progress.complete total in
                    WorkDoneProgressReport.create ~percentage ~message ())))))
;;
