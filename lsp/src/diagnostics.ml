open Import

module Tag = struct
  type t =
    | Unnecessary
    | Deprecated

  let yojson_of_t = function
    | Unnecessary -> `Int 1
    | Deprecated -> `Int 2

  let t_of_yojson = function
    | `Int 1 -> Unnecessary
    | `Int 2 -> Deprecated
    | json -> Json.error "DiagnosticTag" json
end
