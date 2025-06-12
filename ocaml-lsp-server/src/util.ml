open Import

type 't req_params_spec =
  { params_schema : Jsonrpc.Structured.t
  ; of_jsonrpc_params : Jsonrpc.Structured.t -> 't option
  }

let of_jsonrpc_params_exn spec params =
  let raise_invalid_params ?data ~message () =
    Jsonrpc.Response.Error.raise
    @@ Jsonrpc.Response.Error.make
         ?data
         ~code:Jsonrpc.Response.Error.Code.InvalidParams
         ~message
         ()
  in
  match params with
  | None -> raise_invalid_params ~message:"Expected params but received none" ()
  | Some params ->
    (match spec.of_jsonrpc_params params with
     | Some t -> t
     | None ->
       let error_json =
         `Assoc
           [ "params_expected", (spec.params_schema :> Json.t)
           ; "params_received", (params :> Json.t)
           ]
       in
       raise_invalid_params ~message:"Unexpected parameter format" ~data:error_json ())
;;

let language_id_of_fname s =
  match Filename.extension s with
  | ".mli" | ".eliomi" -> "ocaml.interface"
  | ".ml" | ".eliom" -> "ocaml"
  | ".rei" | ".re" -> "reason"
  | ".mll" -> "ocaml.ocamllex"
  | ".mly" -> "ocaml.menhir"
  | ext -> Code_error.raise "unsupported file extension" [ "extension", String ext ]
;;

let open_document_from_file (state : State.t) uri =
  let open Fiber.O in
  let filename = Uri.to_path uri in
  Fiber.of_thunk (fun () ->
    let text = Io.String_path.read_file filename in
    let languageId = language_id_of_fname filename in
    let text_document = TextDocumentItem.create ~uri ~languageId ~version:0 ~text in
    let params = DidOpenTextDocumentParams.create ~textDocument:text_document in
    let+ doc =
      let position_encoding = State.position_encoding state in
      Document.make
        ~position_encoding
        (State.wheel state)
        state.merlin_config
        state.merlin
        params
    in
    Some doc)
;;

let is_at_cursor position =
  let (`Logical (cursor_line, cursor_col)) = Position.logical position in
  let is_at_cursor ({ loc_start; loc_end; _ } : Ocaml_parsing.Location.t) =
    let start_col = loc_start.pos_cnum - loc_start.pos_bol in
    let end_col = loc_end.pos_cnum - loc_end.pos_bol in
    let at_or_after_start =
      loc_start.pos_lnum < cursor_line
      || (loc_start.pos_lnum = cursor_line && start_col <= cursor_col)
    in
    let before_or_at_end =
      loc_end.pos_lnum > cursor_line
      || (loc_end.pos_lnum = cursor_line && end_col >= cursor_col)
    in
    at_or_after_start && before_or_at_end
  in
  is_at_cursor
;;
