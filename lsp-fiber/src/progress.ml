open Import
open Fiber.O
module Progress = Lsp.Progress

include struct
  open Lsp.Types
  module ProgressToken = ProgressToken
  module ProgressParams = ProgressParams
  module WorkDoneProgressCreateParams = WorkDoneProgressCreateParams
  module WorkDoneProgressBegin = WorkDoneProgressBegin
  module WorkDoneProgressEnd = WorkDoneProgressEnd
  module WorkDoneProgressReport = WorkDoneProgressReport
end

module Task = struct
  type t = { token : ProgressToken.t }
end

type t =
  { create_task : WorkDoneProgressCreateParams.t -> unit Fiber.t
  ; report_progress : Progress.t ProgressParams.t -> unit Fiber.t
  ; mutable next_token_id : int
  }

let create ~create_task ~report_progress () =
  { create_task; report_progress; next_token_id = 0 }
;;

let start t ~token_name ~title ?message () =
  let token = `String (sprintf "%s-%d" token_name t.next_token_id) in
  t.next_token_id <- t.next_token_id + 1;
  let* () = t.create_task (WorkDoneProgressCreateParams.create ~token) in
  let+ () =
    t.report_progress
      (ProgressParams.create
         ~token
         ~value:(Progress.Begin (WorkDoneProgressBegin.create ~title ?message ())))
  in
  { Task.token }
;;

let report t { Task.token } ~percentage ~message =
  t.report_progress
    (ProgressParams.create
       ~token
       ~value:(Progress.Report (WorkDoneProgressReport.create ~percentage ~message ())))
;;

let end_ t { Task.token } ~message =
  t.report_progress
    (ProgressParams.create
       ~token
       ~value:(Progress.End (WorkDoneProgressEnd.create ~message ())))
;;
