open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/jump"
let capability = "handleJump", `Bool true

module Jump = struct
  type t = (string * Position.t) list

  let yojson_of_t (lst : t) : Yojson.Safe.t =
    let jumps =
      List.map
        ~f:(fun (target, position) ->
          `Assoc [ "target", `String target; "position", Position.yojson_of_t position ])
        lst
    in
    `Assoc [ "jumps", `List jumps ]
  ;;
end

type t = Jump.t

module Request_params = struct
  type t = TextDocumentPositionParams.t

  let yojson_of_t t = TextDocumentPositionParams.yojson_of_t t

  let create ~uri ~position =
    TextDocumentPositionParams.create
      ~position
      ~textDocument:(TextDocumentIdentifier.create ~uri)
  ;;
end

let dispatch ~merlin ~position =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let position = Mpipeline.get_lexing_pos pipeline position in
    let typedtree = Mpipeline.typer_result pipeline |> Mtyper.get_typedtree in
    Merlin_analysis.Jump.get_all typedtree position)
;;

let on_request ~params state =
  let open Fiber.O in
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let params = TextDocumentPositionParams.t_of_yojson params in
    let uri = params.textDocument.uri in
    let position = Position.logical params.position in
    let doc = Document_store.get state.State.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin ->
      let+ res = dispatch ~merlin ~position in
      Jump.yojson_of_t
        (List.filter_map res ~f:(fun (target, position) ->
           match Position.of_lexical_position position with
           | Some pos -> Some (target, pos)
           | None -> None)))
;;
