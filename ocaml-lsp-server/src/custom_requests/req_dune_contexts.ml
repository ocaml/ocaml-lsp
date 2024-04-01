open Import
open Fiber.O

let capability = ("handleDuneContexts", `Bool true)

let meth = "ocamllsp/duneContexts"

module Request_params = struct
  let parse_exn (params : Jsonrpc.Structured.t option) : unit =
    let raise_invalid_params ?data ~message () =
      Jsonrpc.Response.Error.raise
      @@ Jsonrpc.Response.Error.make
           ?data
           ~code:Jsonrpc.Response.Error.Code.InvalidParams
           ~message
           ()
    in
    match params with
    | None -> ()
    | Some params ->
      let error_json = `Assoc [ ("params_received", (params :> Json.t)) ] in
      raise_invalid_params
        ~message:"Parameters not expected, but received some:"
        ~data:error_json
        ()
end

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
      let () = Request_params.parse_exn params in
      let+ res =
        Merlin_config.dune_contexts
          (Merlin_config.DB.get
             state.merlin_config
             (State.workspace_root state))
      in
      match res with
      | Error (Unexpected_output msg) ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make
             ~code:Jsonrpc.Response.Error.Code.RequestFailed
             ~message:(sprintf "Unexpected output when reading dune contexts")
             ~data:
               (`String
                 (sprintf
                    "'GetContexts' query to ocaml-merlin returned error: %s"
                    msg))
             ())
      | Error (Csexp_parse_error msg) ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make
             ~code:Jsonrpc.Response.Error.Code.RequestFailed
             ~message:(sprintf "Parse error when reading dune contexts")
             ~data:
               (`String
                 (sprintf
                    "'GetContexts' query to ocaml-merlin returned error: %s"
                    msg))
             ())
      | Ok contexts -> Json.yojson_of_list (fun ctxt -> `String ctxt) contexts)
