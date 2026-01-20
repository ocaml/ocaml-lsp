open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Destruct

module Util = struct
  let call_destruct client range =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create ~text_document ~range ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let test pos source =
    let range =
      match pos with
      | `Pos start ->
        Range.create ~start ~end_:Position.{ start with character = start.character + 1 }
      | `Range range -> range
    in
    let request client =
      let open Fiber.O in
      let+ response = call_destruct client range in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Perform `destruct` as custom request - 1" =
  let source =
    {|
let _ =
  match (None : unit option) with
    | None -> ()
    | Some _ -> ()
|}
  in
  let pos = Position.create ~line:4 ~character:11 in
  Util.test (`Pos pos) source;
  [%expect
    {|
    {
      "range": {
        "end": { "character": 12, "line": 4 },
        "start": { "character": 11, "line": 4 }
      },
      "content": "()"
    }
    |}]
;;

let%expect_test "Perform `destruct` as custom request - 2" =
  let source =
    {|
type t =
  | Foo
  | Bar
  | Baz of int option
let f: t -> unit = function Foo -> ()
|}
  in
  let pos =
    let start = Position.create ~line:5 ~character:28
    and end_ = Position.create ~line:5 ~character:31 in
    Range.create ~start ~end_
  in
  Util.test (`Range pos) source;
  [%expect
    {|
    {
      "range": {
        "end": { "character": 37, "line": 5 },
        "start": { "character": 37, "line": 5 }
      },
      "content": "\n| Bar | Baz _ -> _"
    }
    |}]
;;
