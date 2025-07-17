open Import

let capability = "handleRefactorExtract", `Bool true
let meth = "ocamllsp/refactorExtract"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; start : Position.t
    ; stop : Position.t
    ; extract_name : string option
    }

  let create ?extract_name ~text_document ~start ~stop () =
    { text_document; start; stop; extract_name }
  ;;

  let yojson_of_t { text_document; start; stop; extract_name } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let start = "start", Position.yojson_of_t start in
      let stop = "stop", Position.yojson_of_t stop in
      let extract_name =
        "extract_name", Option.fold extract_name ~init:`Null ~f:(fun _ s -> `String s)
      in
      `Assoc (start :: stop :: extract_name :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = json |> TextDocumentIdentifier.t_of_yojson in
    let start = json |> member "start" |> Position.t_of_yojson in
    let stop = json |> member "stop" |> Position.t_of_yojson in
    let extract_name = json |> member "extract_name" |> to_string_option in
    create ?extract_name ~text_document ~start ~stop ()
  ;;
end

type t =
  { position : Range.t
  ; content : string
  ; selection_range : Range.t
  }

let yojson_of_t { position; content; selection_range } =
  `Assoc
    [ "position", Range.yojson_of_t position
    ; "content", `String content
    ; "selection_range", Range.yojson_of_t selection_range
    ]
;;

let with_pipeline state uri f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return `Null
  | `Merlin merlin ->
    (match Document.Merlin.kind merlin with
     | Document.Kind.Intf ->
       (* Extraction makes no sense if its called from an interface. *)
       Fiber.return `Null
     | Document.Kind.Impl -> Document.Merlin.with_pipeline_exn merlin f)
;;

let dispatch ~start ~stop ~extract_name pipeline =
  let start = Position.logical start in
  let end_ = Position.logical stop in
  let buffer = Mpipeline.raw_source pipeline in
  let command =
    Query_protocol.Refactor_extract_region (start, end_, extract_name, buffer)
  in
  let { Query_protocol.loc; content; selection_range } =
    Query_commands.dispatch pipeline command
  in
  yojson_of_t
    { position = Range.of_loc loc
    ; content
    ; selection_range = Range.of_loc selection_range
    }
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let Request_params.{ text_document; start; stop; extract_name } =
      Request_params.t_of_yojson params
    in
    let uri = text_document.uri in
    with_pipeline state uri @@ dispatch ~start ~stop ~extract_name)
;;
