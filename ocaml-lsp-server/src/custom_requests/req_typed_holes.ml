open Import

let capability = ("handleTypedHoles", `Bool true)

let meth = "ocamllsp/typedHoles"

let failwith_err err = failwith @@ Printf.sprintf "%s: %s" meth err

module Request_params = struct
  type t = Uri.t

  (* Request params must have the form as in the given string. *)
  let docstring = "{ uri: <DocumentUri> }"

  let t_of_structured_json params : t option =
    match params with
    | `Assoc [ ("uri", uri) ] ->
      let uri = Uri.t_of_yojson uri in
      Some uri
    | _ -> None

  let parse_exn (params : Jsonrpc.Message.Structured.t option) : t =
    match params with
    | None -> failwith_err "Expected a paramater, but didn't get one"
    | Some params -> (
      match t_of_structured_json params with
      | Some uri -> uri
      | None ->
        Printf.ksprintf failwith_err "Expected a parameter %s, but got %s"
          docstring
          (Json.to_string (params :> Json.t)))
end

let on_request ~(params : Jsonrpc.Message.Structured.t option) (state : State.t)
    =
  let uri = Request_params.parse_exn params in
  let store = state.store in
  let doc = Document_store.get_opt store uri in
  match doc with
  | None ->
    Printf.ksprintf failwith_err
      "Document %s wasn't found in the document store" (Uri.to_string uri)
  | Some doc ->
    let open Fiber.O in
    let+ holes = Document.dispatch_exn doc Holes in
    Json.yojson_of_list
      (fun (loc, _type) -> loc |> Range.of_loc |> Range.yojson_of_t)
      holes
