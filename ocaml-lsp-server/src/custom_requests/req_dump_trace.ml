open Import

let meth = "ocamllsp/dumpTrace"

let on_request ~params:_ state =
  Option.map state.State.tracer ~f:(fun tracer ->
      let events : Json.t list =
        Tracer.events tracer
        |> List.map ~f:(fun e -> (Chrome_trace.Event.to_json e :> Json.t))
      in
      `List events)
  |> Option.value ~default:(`Assoc [])
  (* TODO: if we don't have a tracer (it's None), we should raise with a
     meaningful error rather than returning an empty object *)
  |> Fiber.return
