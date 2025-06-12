open Import
open Fiber.O

let capability = "handleHoverExtended", `Bool true
let meth = "ocamllsp/hoverExtended"

let get_doc_id ~(params : Jsonrpc.Structured.t option) =
  match params with
  | Some (`Assoc params) ->
    List.assoc_opt "textDocument" params
    |> Option.map ~f:TextDocumentIdentifier.t_of_yojson
  | _ -> None
;;

let get_pos ~(params : Jsonrpc.Structured.t option) =
  match params with
  | Some (`Assoc params) ->
    List.assoc_opt "position" params |> Option.map ~f:Position.t_of_yojson
  | _ -> None
;;

module Request_params = struct
  open Json.Conv

  type t =
    { text_document : TextDocumentIdentifier.t [@key "textDocument"]
    ; cursor_position : Position.t [@key "position"]
    ; verbosity : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let create ?verbosity ~text_document ~cursor_position () =
    { text_document; cursor_position; verbosity }
  ;;

  let params_schema =
    `Assoc
      [ "textDocument", `String "<TextDocumentIdentifier>"
      ; "position", `String "<Position>"
      ; "verbosity", `String "<integer?>"
      ]
  ;;

  let of_jsonrpc_params params =
    try Some (t_of_yojson (Jsonrpc.Structured.yojson_of_t params)) with
    | _exn -> None
  ;;

  let of_jsonrpc_params_exn params : t =
    let params_spec = Util.{ params_schema; of_jsonrpc_params } in
    Util.of_jsonrpc_params_exn params_spec params
  ;;
end

type t = Hover.t

let t_of_yojson = Hover.t_of_yojson

module Response_json = struct
  open Lsp.Types

  type t = Hover_req.extended_hover =
    { hover : Hover.t (* Extended hover extensions *)
    ; verbosity : int
    ; can_increase_verbosity : bool
    }

  let yojson_of_t { hover; verbosity; can_increase_verbosity } =
    match Hover.yojson_of_t hover with
    | `Assoc fields ->
      let extended =
        ("verbosity", yojson_of_int verbosity)
        :: ("canIncreaseVerbosity", `Bool can_increase_verbosity)
        :: ("canDecreaseVerbosity", `Bool (verbosity > 0))
        :: fields
      in
      `Assoc extended
    | _ -> failwith "Couldn't create yojson for extended hover."
  ;;
end

let on_request
  ~log_info
  ~(params : Jsonrpc.Structured.t option)
  (server : State.t Server.t)
  =
  let { Request_params.text_document; cursor_position; verbosity } =
    Request_params.of_jsonrpc_params_exn params
  in
  let+ res =
    Hover_req.handle_extended
      ~log_info
      server
      { HoverParams.textDocument = text_document
      ; position = cursor_position
      ; workDoneToken = None
      }
      ~verbosity
  in
  match res with
  | None -> `Null
  | Some res -> Response_json.yojson_of_t res
;;
