open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Jump_typed_hole

module Util = struct
  let call ?direction ?range ~position client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let meth = Req.meth in
    let params =
      Req.Request_params.create ?direction ?range ~text_document ~position ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth; params } in
    Client.request client req
  ;;

  let test ?direction ?range ~line ~character source =
    let position = Position.create ~line ~character in
    let request client =
      client |> call ?direction ?range ~position |> Fiber.map ~f:Test.print_result
    in
    Helpers.test source request
  ;;
end

let range (la, ca) (lb, cb) =
  let start = Position.create ~line:la ~character:ca
  and end_ = Position.create ~line:lb ~character:cb in
  Range.create ~start ~end_
;;

let zip_shortest_source =
  {ocaml|
let rec zip_shortest xs ys =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
;;

let%expect_test "when there is no hole" =
  let source =
    {ocaml|
let f = List.map (fun x -> x + 1) [1; 2; 3]
|ocaml}
  and direction = `Next
  and line = 1
  and character = 1 in
  Util.test ~direction ~line ~character source;
  [%expect {| null |}]
;;

let%expect_test "jump to next hole 1" =
  let source = zip_shortest_source
  and direction = `Next
  and line = 4
  and character = 40 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 48, "line": 4 },
      "start": { "character": 47, "line": 4 }
    }
    |}]
;;

let%expect_test "jump to prev hole 1" =
  let source = zip_shortest_source
  and direction = `Prev
  and line = 4
  and character = 40 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 28, "line": 4 },
      "start": { "character": 27, "line": 4 }
    }
    |}]
;;

let%expect_test "jump to next hole can wrap" =
  let source = zip_shortest_source
  and direction = `Next
  and line = 5
  and character = 1 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 43, "line": 3 },
      "start": { "character": 42, "line": 3 }
    }
    |}]
;;

let%expect_test "jump to prev hole can wrap" =
  let source = zip_shortest_source
  and direction = `Prev
  and line = 2
  and character = 20 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 50, "line": 4 },
      "start": { "character": 49, "line": 4 }
    }
    |}]
;;

let%expect_test "can jump to next hole when on char before current hole" =
  let source = zip_shortest_source
  and direction = `Next
  and line = 4
  and character = 27 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 48, "line": 4 },
      "start": { "character": 47, "line": 4 }
    }
    |}]
;;

let%expect_test "can jump to next hole when on char after current hole" =
  let source = zip_shortest_source
  and direction = `Next
  and line = 4
  and character = 28 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 48, "line": 4 },
      "start": { "character": 47, "line": 4 }
    }
    |}]
;;

let%expect_test "can jump to prev hole when on char before current hole" =
  let source = zip_shortest_source
  and direction = `Prev
  and line = 4
  and character = 27 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 25, "line": 4 },
      "start": { "character": 24, "line": 4 }
    }
    |}]
;;

let%expect_test "can jump to prev hole when on char after current hole" =
  let source = zip_shortest_source
  and direction = `Prev
  and line = 4
  and character = 28 in
  Util.test ~direction ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 25, "line": 4 },
      "start": { "character": 24, "line": 4 }
    }
    |}]
;;

let%expect_test "next hole in a given range" =
  let source = zip_shortest_source
  and direction = `Next
  and range = range (4, 22) (4, 51)
  and line = 4
  and character = 28 in
  Util.test ~direction ~range ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 48, "line": 4 },
      "start": { "character": 47, "line": 4 }
    }
    |}]
;;

let%expect_test "prev hole in a given range" =
  let source = zip_shortest_source
  and direction = `Prev
  and range = range (4, 22) (4, 51)
  and line = 4
  and character = 29 in
  Util.test ~direction ~range ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 28, "line": 4 },
      "start": { "character": 27, "line": 4 }
    }
    |}]
;;

let%expect_test "next hole in a given range with cursor outside the range" =
  let source = zip_shortest_source
  and direction = `Next
  and range = range (4, 22) (4, 51)
  and line = 3
  and character = 1 in
  Util.test ~direction ~range ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 25, "line": 4 },
      "start": { "character": 24, "line": 4 }
    }
    |}]
;;

let%expect_test "prev hole in a given range with cursor outside the range" =
  let source = zip_shortest_source
  and direction = `Prev
  and range = range (4, 22) (4, 51)
  and line = 3
  and character = 1 in
  Util.test ~direction ~range ~line ~character source;
  [%expect
    {|
    {
      "end": { "character": 50, "line": 4 },
      "start": { "character": 49, "line": 4 }
    }
    |}]
;;
