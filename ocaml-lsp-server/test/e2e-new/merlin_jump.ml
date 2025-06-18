open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Merlin_jump

module Util = struct
  let call_jump position ?target client =
    let uri = DocumentUri.of_path "test.ml" in
    let params =
      Req.Request_params.create ~uri ~position ~target
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = "ocamllsp/jump"; params } in
    Client.request client req
  ;;

  let test ~line ~character ~source ?target () =
    let position = Position.create ~character ~line in
    let request client =
      let open Fiber.O in
      let+ response = call_jump position client ?target in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Get all jumps including the next match case" =
  let source =
    {|
let find_vowel x =
match x with
| 'A' -> true
| 'E' -> true
| 'I' -> true
| 'O' -> true
| 'U' -> true
| _ -> false
|}
  in
  let line = 3 in
  let character = 2 in
  Util.test ~line ~character ~source ();
  [%expect
    {|
  {
    "jumps": [
      { "target": "fun", "position": { "character": 0, "line": 1 } },
      { "target": "match", "position": { "character": 0, "line": 2 } },
      { "target": "let", "position": { "character": 0, "line": 1 } },
      {
        "target": "match-next-case",
        "position": { "character": 2, "line": 4 }
      }
    ]
  } |}]
;;

let%expect_test "Get location of the next match case" =
  let source =
    {|
let find_vowel x =
match x with
| 'A' -> true
| 'E' -> true
| 'I' -> true
| 'O' -> true
| 'U' -> true
| _ -> false
|}
  in
  let line = 3 in
  let character = 2 in
  Util.test ~line ~character ~source ~target:"match-next-case" ();
  [%expect
    {|
    {
      "jumps": [
        {
          "target": "match-next-case",
          "position": { "character": 2, "line": 4 }
        }
      ]
    }
    |}]
;;

let%expect_test "Get location of a the module" =
  let source =
    {|type a = Foo | Bar

module A = struct
  let f () = 10
  let g = Bar
  let h x = x

  module B = struct
    type b = Baz

    let x = (Baz, 10)
    let y = (Bar, Foo)
  end

  type t = { a : string; b : float }

  let z = { a = "Hello"; b = 1.0 }
end|}
  in
  let line = 10 in
  let character = 3 in
  Util.test ~line ~character ~source ();
  [%expect
    {|
  {
    "jumps": [
      { "target": "module", "position": { "character": 2, "line": 7 } }
    ]
  } |}]
;;

let%expect_test "Same line should output no locations" =
  let source = {|let x = 5 |} in
  let line = 1 in
  let character = 5 in
  Util.test ~line ~character ~source ();
  [%expect {| { "jumps": [] } |}]
;;

let%expect_test "Ask for a non-existing target" =
  let source =
    {|
let find_vowel x = ()
|}
  in
  let line = 1 in
  let character = 2 in
  Util.test ~line ~character ~source ~target:"notatarget" ();
  [%expect {| { "jumps": [] } |}]
;;
