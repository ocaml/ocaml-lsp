open Import

let capability = "handleJumpTypedHole", `Bool true
let meth = "ocamllsp/jumpTypedHole"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; range : Range.t option
    ; direction : [ `Next | `Prev ]
    }

  let create ?(direction = `Next) ?range ~text_document ~position () =
    { text_document; position; direction; range }
  ;;

  let yojson_of_direction = function
    | `Next -> `String "next"
    | `Prev -> `String "prev"
  ;;

  let yojson_of_t { text_document; position; direction; range } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let position = "position", Position.yojson_of_t position in
      let range =
        ( "range"
        , match range with
          | None -> `Null
          | Some r -> Range.yojson_of_t r )
      in
      let direction = "direction", yojson_of_direction direction in
      `Assoc (direction :: position :: range :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let direction_of_yojson json =
    let open Yojson.Safe.Util in
    let dir = json |> member "direction" |> to_string in
    match String.lowercase_ascii dir with
    | "prev" -> `Prev
    | _ -> `Next
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = TextDocumentIdentifier.t_of_yojson json in
    let position = json |> member "position" |> Position.t_of_yojson in
    let direction = direction_of_yojson json in
    let range = json |> member "range" |> to_option Range.t_of_yojson in
    { text_document; position; direction; range }
  ;;
end

type t = Range.t option

let t_of_yojson opt =
  let open Yojson.Safe.Util in
  to_option Range.t_of_yojson opt
;;

let yojson_of_t = function
  | None -> `Null
  | Some range -> Range.yojson_of_t range
;;

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Json.t) in
    let Request_params.{ text_document = { uri }; position; direction; range } =
      Request_params.t_of_yojson params
    in
    match Document_store.get_opt state.store uri with
    | Some doc ->
      let open Fiber.O in
      let merlin = Document.merlin_exn doc in
      let+ holes = Typed_hole.all ~name:"jump-to-typed-hole" merlin in
      holes |> Typed_hole.find ~position ~range ~direction |> yojson_of_t
    | None ->
      Jsonrpc.Response.Error.raise
      @@ Jsonrpc.Response.Error.make
           ~code:Jsonrpc.Response.Error.Code.InvalidParams
           ~message:
             (Printf.sprintf
                "Document %s wasn't found in the document store"
                (Uri.to_string uri))
           ())
;;
