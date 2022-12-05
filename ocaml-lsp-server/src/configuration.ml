open! Import
open Fiber.O

type t = { wheel : Lev_fiber.Timer.Wheel.t }

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
  { wheel }

let update t { DidChangeConfigurationParams.settings } =
  match
    match settings with
    | `Assoc xs -> (
      match List.assoc xs "diagnostics_delay" with
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
  | None -> Fiber.return t
  | Some delay ->
    if Float.equal delay (Lev_fiber.Timer.Wheel.delay t.wheel) then
      Fiber.return t
    else
      let+ () = Lev_fiber.Timer.Wheel.set_delay t.wheel ~delay in
      t
