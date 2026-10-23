open Import
open Fiber.O

let capability = "handleCompletePrefixAtPos", `Bool true
let meth = "ocamllsp/completePrefixAtPos"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; prefix : string
    }

  let create ~text_document ~position ~prefix = { text_document; position; prefix }

  let yojson_of_t { text_document; position; prefix } =
    `Assoc
      [ "textDocument", TextDocumentIdentifier.yojson_of_t text_document
      ; "position", Position.yojson_of_t position
      ; "prefix", `String prefix
      ]
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document =
      json |> member "textDocument" |> TextDocumentIdentifier.t_of_yojson
    in
    let position = json |> member "position" |> Position.t_of_yojson in
    let prefix = json |> member "prefix" |> to_string in
    create ~text_document ~position ~prefix
  ;;

  let params_schema =
    `Assoc
      [ "textDocument", `String "<TextDocumentIdentifier>"
      ; "position", `String "<Position>"
      ; "prefix", `String "<string>"
      ]
  ;;

  let of_jsonrpc_params params =
    try Some (t_of_yojson (Jsonrpc.Structured.yojson_of_t params)) with
    | _ -> None
  ;;

  let of_jsonrpc_params_exn params =
    let params_spec = Util.{ params_schema; of_jsonrpc_params } in
    Util.of_jsonrpc_params_exn params_spec params
  ;;
end

let serialize (completions, labels) =
  let entries = "entries", `List (List.map completions ~f:CompletionItem.yojson_of_t) in
  match labels with
  | None -> `Assoc [ entries ]
  | Some labels ->
    `Assoc
      [ entries
      ; ( "context"
          (* The complex, seemingly redundant structure here is intended to mirror
             the output of 'merlin complete-prefix'. One day there may be forms of
             context other than just function application. *)
        , `List
            [ `String "application"
            ; `Assoc
                [ ( "labels"
                  , `List
                      (List.map labels ~f:(fun (n, t) ->
                         `Assoc [ "name", `String n; "type", `String t ])) )
                ]
            ] )
      ]
;;

let on_request ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let Request_params.{ text_document = { uri }; position; prefix } =
      Request_params.of_jsonrpc_params_exn params
    in
    let doc = Document_store.get state.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return (`Assoc [ "entries", `Null ])
    | `Merlin merlin ->
      Compl.Complete_by_prefix.complete_nosquash merlin ~prefix position >>| serialize)
;;

module For_testing = struct
  let serialize = serialize
end
