open Import
open Fiber.O

let capability = "handleHoverExtended", `Bool true
let meth = "ocamllsp/hoverExtended"

module Request_params = struct
  open Json.Conv

  type t =
    { text_document : TextDocumentIdentifier.t [@key "textDocument"]
    ; cursor_position : Position.t [@key "position"]
    ; verbosity : int Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let create ?verbosity ~text_document ~cursor_position () =
    { text_document; cursor_position; verbosity }
  ;;

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    (let _tp_loc =
       "ocaml-lsp-server/src/custom_requests/req_hover_extended.ml.Request_params.t"
     in
     function
     | `Assoc field_yojsons as yojson ->
       let text_document_field = ref Ppx_yojson_conv_lib.Option.None
       and cursor_position_field = ref Ppx_yojson_conv_lib.Option.None
       and verbosity_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter = function
         | (field_name, _field_yojson) :: tail ->
           (match field_name with
            | "textDocument" ->
              (match Ppx_yojson_conv_lib.( ! ) text_document_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                 text_document_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | "position" ->
              (match Ppx_yojson_conv_lib.( ! ) cursor_position_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue = Position.t_of_yojson _field_yojson in
                 cursor_position_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | "verbosity" ->
              (match Ppx_yojson_conv_lib.( ! ) verbosity_field with
               | Ppx_yojson_conv_lib.Option.None ->
                 let fvalue =
                   Json.Nullable_option.t_of_yojson int_of_yojson _field_yojson
                 in
                 verbosity_field := Ppx_yojson_conv_lib.Option.Some fvalue
               | Ppx_yojson_conv_lib.Option.Some _ ->
                 duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates)
            | _ -> ());
           iter tail
         | [] -> ()
       in
       iter field_yojsons;
       (match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
            _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] ->
          (match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] ->
             (match
                ( Ppx_yojson_conv_lib.( ! ) text_document_field
                , Ppx_yojson_conv_lib.( ! ) cursor_position_field
                , Ppx_yojson_conv_lib.( ! ) verbosity_field )
              with
              | ( Ppx_yojson_conv_lib.Option.Some text_document_value
                , Ppx_yojson_conv_lib.Option.Some cursor_position_value
                , verbosity_value ) ->
                { text_document = text_document_value
                ; cursor_position = cursor_position_value
                ; verbosity =
                    (match verbosity_value with
                     | Ppx_yojson_conv_lib.Option.None -> None
                     | Ppx_yojson_conv_lib.Option.Some v -> v)
                }
              | _ ->
                Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                  _tp_loc
                  yojson
                  [ ( Ppx_yojson_conv_lib.poly_equal
                        (Ppx_yojson_conv_lib.( ! ) text_document_field)
                        Ppx_yojson_conv_lib.Option.None
                    , "text_document" )
                  ; ( Ppx_yojson_conv_lib.poly_equal
                        (Ppx_yojson_conv_lib.( ! ) cursor_position_field)
                        Ppx_yojson_conv_lib.Option.None
                    , "cursor_position" )
                  ])))
     | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc yojson
     : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
  ;;

  let _ = t_of_yojson

  let yojson_of_t =
    (function
     | { text_document = v_text_document
       ; cursor_position = v_cursor_position
       ; verbosity = v_verbosity
       } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         if None = v_verbosity
         then bnds
         else (
           let arg = (Json.Nullable_option.yojson_of_t yojson_of_int) v_verbosity in
           let bnd = "verbosity", arg in
           bnd :: bnds)
       in
       let bnds =
         let arg = Position.yojson_of_t v_cursor_position in
         ("position", arg) :: bnds
       in
       let bnds =
         let arg = TextDocumentIdentifier.yojson_of_t v_text_document in
         ("textDocument", arg) :: bnds
       in
       `Assoc bnds
     : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
  ;;

  let _ = yojson_of_t

  [@@@end]

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

let on_request ~(params : Jsonrpc.Structured.t option) (server : State.t Server.t) =
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
;;
