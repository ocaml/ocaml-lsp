open Import

let capability = "handleDestruct", `Bool true
let meth = "ocamllsp/destruct"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; range : Range.t
    }

  let create ~text_document ~range () = { text_document; range }

  let yojson_of_t { text_document; range } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc -> `Assoc (("range", Range.yojson_of_t range) :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = json |> TextDocumentIdentifier.t_of_yojson in
    let range = json |> member "range" |> Range.t_of_yojson in
    create ~text_document ~range ()
  ;;
end

type t =
  { range : Range.t
  ; content : string
  }

let t_of_yojson json =
  let open Yojson.Safe.Util in
  let range = json |> member "range" |> Range.t_of_yojson
  and content = json |> member "content" |> to_string in
  { range; content }
;;

let yojson_of_t { range; content } =
  `Assoc [ "range", Range.yojson_of_t range; "content", `String content ]
;;

let make_destruct_command start stop = Query_protocol.Case_analysis (start, stop)

let dispatch_destruct range pipeline =
  let start = range.Range.start |> Position.logical
  and stop = range.Range.end_ |> Position.logical in
  let command = make_destruct_command start stop in
  let loc, content = Query_commands.dispatch pipeline command in
  yojson_of_t { content; range = Range.of_loc loc }
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Json.t) in
    let Request_params.{ text_document; range } = Request_params.t_of_yojson params in
    let uri = text_document.uri in
    Util.with_impl_pipeline state uri ~default:`Null @@ dispatch_destruct range)
;;
