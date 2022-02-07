open Import

type t = bool

let of_opt_json (json : Json.t option) =
  match json with
  | Some (`Assoc fields) ->
    Json.field fields "jumpToNextHole" Json.Conv.bool_of_yojson
    |> Option.value ~default:false
  | _ -> false

let supportsJumpToNextHole t = t
