open Import
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

