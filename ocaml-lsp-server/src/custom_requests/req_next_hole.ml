open Import

let capability = ("handleNextHole", `Bool true)

let meth = "ocamllsp/nextHole"

type request_params =
  { uri : Uri.t
  ; position : Position.t
  }

(* Request params must have the form as in the given string. *)
let params_s = "{ uri: <DocumentUri>, position: <Position> }"

let parse_params params =
  match params with
  | `Assoc [ ("uri", uri); ("position", position) ] ->
    let uri = Uri.t_of_yojson uri in
    let position = Position.t_of_yojson position in
    Some { uri; position }
  | _ -> None

let failwith_err err = failwith @@ Printf.sprintf "%s: %s" meth err

let on_request ~(params : Jsonrpc.Message.Structured.t option) (state : State.t)
    =
  match params with
  | None -> failwith_err "Expected a paramater, but didn't get one"
  | Some params -> (
    match parse_params params with
    | None ->
      failwith_err
      @@ Printf.sprintf "Expected a parameter %s, but got %s" params_s
           (Json.to_string (params :> Json.t))
    | Some { uri; position = { line; character } } -> (
      let uri_s = Uri.to_string uri in
      let store = state.store in
      let doc = Document_store.get_opt store uri in
      match doc with
      | None ->
        failwith_err
        @@ Printf.sprintf "Document %s wasn't found in the document store" uri_s
      | Some doc -> (
        let open Fiber.O in
        let* range =
          Holes_cmd.next_hole doc @@ Position.create ~line ~character
        in
        match range with
        | Some range -> range |> Range.yojson_of_t |> Fiber.return
        | None -> Fiber.return `Null)))
