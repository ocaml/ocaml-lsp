open Import
open Fiber.O

let capability = "handleTypedHoles", `Bool true
let jump_capability = "jumpToHole", `Bool true
let meth = "ocamllsp/typedHoles"
let jump = "ocamllsp/jumpToHole"

let get_doc_id ~(params : Jsonrpc.Structured.t option) =
  match params with
  | Some (`Assoc params) ->
    List.assoc_opt "uri" params
    |> Option.map ~f:(fun (param : Json.t) ->
      let uri = DocumentUri.t_of_yojson param in
      { TextDocumentIdentifier.uri })
  | _ -> None
;;

let get_pos ~(params : Jsonrpc.Structured.t option) =
  match params with
  | Some (`Assoc params) ->
    List.assoc_opt "position" params |> Option.map ~f:Position.t_of_yojson
  | _ -> None
;;

let raise_invalid_params ?data ~message () =
  Jsonrpc.Response.Error.raise
  @@ Jsonrpc.Response.Error.make
       ?data
       ~code:Jsonrpc.Response.Error.Code.InvalidParams
       ~message
       ()
;;

module Request_params = struct
  type t = Uri.t

  (* Request params must have the form as in the given string. *)
  let expected_params = `Assoc [ "uri", `String "<DocumentUri>" ]
  let create uri = uri

  let t_of_structured_json params : t option =
    match params with
    | `Assoc [ ("uri", uri) ] ->
      let uri = Uri.t_of_yojson uri in
      Some uri
    | _ -> None
  ;;

  let parse_exn (params : Jsonrpc.Structured.t option) : t =
    match params with
    | None -> raise_invalid_params ~message:"Expected params but received none" ()
    | Some params ->
      (match t_of_structured_json params with
       | Some uri -> uri
       | None ->
         let error_json =
           `Assoc
             [ "params_expected", expected_params; "params_received", (params :> Json.t) ]
         in
         raise_invalid_params ~message:"Unxpected parameter format" ~data:error_json ())
  ;;

  let yojson_of_t = Uri.yojson_of_t
end

type t = Range.t list

let t_of_yojson list =
  let open Yojson.Safe.Util in
  list |> to_list |> List.map ~f:(fun range -> range |> Range.t_of_yojson)
;;

let get_holes_from_merlin ~log_info ~uri ~(state : State.t) =
  let doc = Document_store.get_opt state.store uri in
  match doc with
  | None ->
    let message =
      Printf.sprintf "Document %s wasn't found in the document store" (Uri.to_string uri)
    in
    raise_invalid_params ~message ()
  | Some doc ->
    let+ holes = Document.Merlin.dispatch_exn ~log_info (Document.merlin_exn doc) Holes in
    List.map ~f:(fun (loc, _type) -> Range.of_loc loc) holes
;;

let on_request ~log_info ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let uri = Request_params.parse_exn params in
    let+ holes = get_holes_from_merlin ~log_info ~uri ~state in
    Json.yojson_of_list Range.yojson_of_t holes)
;;

module Jump_request_params = struct
  type dir =
    | Next
    | Prev

  type t =
    { text_document_uri : Uri.t
    ; cursor_position : Position.t
    ; direction : dir
    }

  let expected_params =
    `Assoc
      [ "uri", `String "<DocumentUri>"
      ; "position", `String "<CursorPosition>"
      ; "direction", `String "prev|next"
      ]
  ;;

  let parse_exn (params : Jsonrpc.Structured.t option) : t =
    match params with
    | None ->
      raise_invalid_params ~message:"Expected params for jumpToHole but received none" ()
    | Some params ->
      (match params with
       | `Assoc [ ("uri", uri); ("position", position); ("direction", `String direction) ]
         ->
         let text_document_uri = Uri.t_of_yojson uri in
         let cursor_position = Position.t_of_yojson position in
         let direction =
           match direction with
           | "next" -> Next
           | "prev" -> Prev
           | _ -> raise_invalid_params ~message:("Invalid direction: " ^ direction) ()
         in
         { text_document_uri; cursor_position; direction }
       | _ ->
         let error_json =
           `Assoc
             [ "params_expected", expected_params; "params_received", (params :> Json.t) ]
         in
         raise_invalid_params
           ~message:"Unxpected parameter format in jumpToHole"
           ~data:error_json
           ())
  ;;
end

let on_jump_request ~log_info ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let params = Jump_request_params.parse_exn params in
    let uri = params.text_document_uri in
    let+ holes = get_holes_from_merlin ~log_info ~uri ~state in
    let cursor = params.cursor_position in
    let jump_dest =
      match params.direction with
      | Next -> Typed_hole.next_hole ~holes ~cursor
      | Prev -> Typed_hole.prev_hole ~holes ~cursor
    in
    match jump_dest with
    | None -> `Null
    | Some jump_dest -> Range.yojson_of_t jump_dest)
;;
