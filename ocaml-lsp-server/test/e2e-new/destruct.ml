open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Destruct

module Util = struct
  let call_destruct client range =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let params =
      Req.Request_params.create ~text_document ~range () |> Req.Request_params.yojson_of_t
    in
    Test.custom_request client Req.meth params
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

let%expect_test "destruct at a non-destructible range raises an internal error" =
  let source = "let x = 1" in
  let request client =
    let position = Position.create ~line:0 ~character:0 in
    let range = Range.create ~start:position ~end_:position in
    let* result = Fiber.collect_errors (fun () -> Util.call_destruct client range) in
    match result with
    | Error
        [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ] ->
      let data = Option.value_exn error.data in
      let exn = Yojson.Safe.Util.(data |> member "exn" |> to_string) in
      Printf.printf
        "code: %s\nexception: %s\n"
        (Jsonrpc.Response.Error.Code.to_string error.code)
        exn;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors
    | Ok response ->
      Test.print_result response;
      Fiber.return ()
  in
  Helpers.test source request;
  [%expect
    {|
    code: InternalError
    exception: Merlin_analysis.Destruct.Not_allowed("value_binding")
    |}]
;;

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
