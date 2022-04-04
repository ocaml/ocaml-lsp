open! Import

type t = { diagnostics_delay : float }

let default =
  { diagnostics_delay =
      (match Sys.getenv_opt "OCAMLLSP_TEST" with
      | None -> 0.25
      | Some _ -> 0.0)
  }

let diagnostics_delay t = t.diagnostics_delay

let request = { ConfigurationParams.items = [] }

let of_response (_ : Json.t list) = default

let update t _ = t
