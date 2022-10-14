open Import
open Fiber.O

let capability = ("handleHoverExtended", `Bool true)

let meth = "ocamllsp/hoverExtended"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; cursor_position : Position.t
    ; verbosity : int Json.Nullable_option.t
          [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  [@@@end]

  let params_schema =
    `Assoc
      [ ("textDocument", `String "<TextDocumentIdentifier>")
      ; ("position", `String "<Position>")
      ; ("verbosity", `String "<integer?>")
      ]

  let of_jsonrpc_params params : t option =
    match params with
    | `Assoc
        [ ("textDocument", text_document)
        ; ("position", position)
        ; ("verbosity", verbosity)
        ] ->
      let text_document = TextDocumentIdentifier.t_of_yojson text_document in
      let cursor_position = Position.t_of_yojson position in
      let verbosity = Some (Yojson.Safe.Util.to_int verbosity) in
      Some { text_document; cursor_position; verbosity }
    | `Assoc [ ("textDocument", text_document); ("position", position) ] ->
      let text_document = TextDocumentIdentifier.t_of_yojson text_document in
      let cursor_position = Position.t_of_yojson position in
      let verbosity = None in
      Some { text_document; cursor_position; verbosity }
    | _ -> None

  let of_jsonrpc_params_exn params : t =
    let params_spec = { Custom_request.params_schema; of_jsonrpc_params } in
    Custom_request.of_jsonrpc_params_exn params_spec params
end

let client_capabilities (state : State.t) =
  (State.initialize_params state).capabilities

let on_request ~(params : Jsonrpc.Structured.t option)
    (server : State.t Server.t) (_state : State.t) =
  let { Request_params.text_document; cursor_position; verbosity } =
    Request_params.of_jsonrpc_params_exn params
  in
  let+ res =
    Hover_req.handle
      server
      { HoverParams.textDocument = text_document
      ; position = cursor_position
      ; workDoneToken = None
      }
      (match verbosity with
      | None -> Hover_req.Extended_variable
      | Some v -> Hover_req.Extended_fixed v)
  in
  match res with
  | None -> `Null
  | Some res -> Hover.yojson_of_t res
