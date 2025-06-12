open! Import
open Fiber.O

type t =
  { wheel : Lev_fiber.Timer.Wheel.t
  ; data : Config_data.t
  }

let wheel t = t.wheel

let default () =
  let+ wheel =
    let delay =
      match Env_vars._TEST () with
      | None -> 0.25
      | Some _ -> 0.0
    in
    Lev_fiber.Timer.Wheel.create ~delay
  in
  let data = Config_data.default in
  { wheel; data }
;;

let update t { DidChangeConfigurationParams.settings } =
  let* wheel =
    match
      match settings with
      | `Assoc xs ->
        (match List.assoc xs "diagnostics_delay" with
         | Some (`Float f) -> Some f
         | Some (`Int i) -> Some (float_of_int i)
         | None -> None
         | _ ->
           Jsonrpc.Response.Error.raise
             (Jsonrpc.Response.Error.make
                ~code:InvalidRequest
                ~message:"invalid value for diagnostics_delay"
                ()))
      | _ -> None
    with
    | None -> Fiber.return t.wheel
    | Some delay ->
      if Float.equal delay (Lev_fiber.Timer.Wheel.delay t.wheel)
      then Fiber.return t.wheel
      else
        let* () = Lev_fiber.Timer.Wheel.set_delay t.wheel ~delay in
        Fiber.return t.wheel
  in
  let data =
    let new_data = Config_data.t_of_yojson settings in
    let merge x y = Option.merge x y ~f:(fun _ y -> y) in
    { Config_data.codelens = merge t.data.codelens new_data.codelens
    ; extended_hover = merge t.data.extended_hover new_data.extended_hover
    ; merlin_diagnostics = merge t.data.merlin_diagnostics new_data.merlin_diagnostics
    ; inlay_hints = merge t.data.inlay_hints new_data.inlay_hints
    ; syntax_documentation =
        merge t.data.syntax_documentation new_data.syntax_documentation
    ; shorten_merlin_diagnostics =
        merge t.data.shorten_merlin_diagnostics new_data.shorten_merlin_diagnostics
    ; ppx_css_colors = merge t.data.ppx_css_colors new_data.ppx_css_colors
    }
  in
  Fiber.return { wheel; data }
;;

let display_merlin_diagnostics t =
  match t.data.merlin_diagnostics with
  | Some { enable = true } -> true
  | Some { enable = false } | None -> false
;;

let shorten_merlin_diagnostics t =
  match t.data.shorten_merlin_diagnostics with
  | Some { enable = true } | None -> true
  | Some { enable = false } -> false
;;
