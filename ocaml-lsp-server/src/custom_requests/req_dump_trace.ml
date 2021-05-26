open Import

let capability = ("handleDumpTrace", `Bool true)

let meth = "ocamllsp/dumpTrace"

let on_request :
    params:Jsonrpc.Message.Structured.t option -> State.t -> Json.t Fiber.t =
 fun ~params:_ state ->
  match state.tracer with
  | None -> failwith "Tracer not activated"
  | Some tracer ->
    let events = Tracer.to_list tracer in
    Json.yojson_of_list Chrome_trace.Event.to_json events |> Fiber.return
