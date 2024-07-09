open Import
open Types

type t =
  | Begin of WorkDoneProgressBegin.t
  | Report of WorkDoneProgressReport.t
  | End of WorkDoneProgressEnd.t

let yojson_of_t = function
  | Begin b -> WorkDoneProgressBegin.yojson_of_t b
  | Report r -> WorkDoneProgressReport.yojson_of_t r
  | End e -> WorkDoneProgressEnd.yojson_of_t e
;;

let t_of_yojson json =
  Json.Of.untagged_union
    "Progress"
    [ (fun j -> Begin (WorkDoneProgressBegin.t_of_yojson j))
    ; (fun j -> Report (WorkDoneProgressReport.t_of_yojson j))
    ; (fun j -> End (WorkDoneProgressEnd.t_of_yojson j))
    ]
    json
;;

let method_ = "$/progress"
