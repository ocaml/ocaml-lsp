open Import

type enabled =
  { (* TODO this needs to be mutexed *)
    mutable token : ProgressToken.t option
  ; mutable build_counter : int
  ; report_progress :
      Server_notification.Progress.t ProgressParams.t -> unit Fiber.t
  ; create_task : WorkDoneProgressCreateParams.t -> unit Fiber.t
  }

type t =
  | Disabled
  | Enabled of enabled

let create (client_capabilities : ClientCapabilities.t) ~report_progress
    ~create_task =
  match client_capabilities.window with
  | Some { workDoneProgress = Some true; _ } ->
    Enabled { token = None; build_counter = 0; create_task; report_progress }
  | _ -> Disabled

let end_build (t : enabled) ~message =
  match t.token with
  | None -> Fiber.return ()
  | Some token ->
    t.token <- None;
    t.report_progress
      (ProgressParams.create ~token
         ~value:
           (Server_notification.Progress.End
              (WorkDoneProgressEnd.create ~message ())))

let end_build_if_running = function
  | Disabled -> Fiber.return ()
  | Enabled e -> end_build e ~message:"Build interrupted"

let start_build (t : enabled) =
  let open Fiber.O in
  let* () = end_build t ~message:"Starting new build" in
  let token = `String ("dune-build-" ^ Int.to_string t.build_counter) in
  t.token <- Some token;
  t.build_counter <- t.build_counter + 1;
  let* () = t.create_task (WorkDoneProgressCreateParams.create ~token) in
  t.token <- Some token;
  let+ () =
    t.report_progress
      (ProgressParams.create ~token
         ~value:
           (Server_notification.Progress.Begin
              (WorkDoneProgressBegin.create ~title:"Build" ~message:"started" ())))
  in
  token

let build_event t (event : Drpc.Build.Event.t) =
  match t with
  | Disabled -> Code_error.raise "progress reporting is not supported" []
  | Enabled t -> (
    match event with
    | Finish -> end_build t ~message:"Build finished"
    | Fail -> end_build t ~message:"Build failed"
    | Interrupt -> end_build t ~message:"Build interrupted"
    | Start ->
      let open Fiber.O in
      let+ (_ : ProgressToken.t) = start_build t in
      ())

let build_progress t (progress : Drpc.Progress.t) =
  match t with
  | Disabled -> Code_error.raise "progress reporting is not supported" []
  | Enabled ({ token; report_progress; _ } as t) ->
    let open Fiber.O in
    let* token =
      match token with
      | Some token -> Fiber.return token
      | None ->
        (* This can happen when we connect to dune in the middle of a build. *)
        start_build t
    in
    let percentage =
      let fraction =
        float_of_int progress.complete
        /. float_of_int (progress.complete + progress.remaining)
      in
      int_of_float (fraction *. 100.)
    in
    report_progress
      (ProgressParams.create ~token
         ~value:
           (Server_notification.Progress.Report
              (WorkDoneProgressReport.create ~percentage ~message:"Building" ())))

let should_report_build_progress = function
  | Disabled -> false
  | Enabled _ -> true
