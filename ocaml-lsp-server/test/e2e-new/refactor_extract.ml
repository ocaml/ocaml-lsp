open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Refactor_extract

module Util = struct
  let call_extract ?extract_name ~start ~stop client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create ?extract_name ~text_document ~start ~stop ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let test ?extract_name ~start ~stop source =
    let request client =
      let open Fiber.O in
      let+ response = call_extract ?extract_name ~start ~stop client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Example sample from merlin 1" =
  let source =
    {|module type EMPTY = sig end
let f () : (module EMPTY) =
  (module struct
    let const_name2 = assert false
    let secret = String.make 100 '@'
  end)
|}
  in
  let start = Position.create ~line:4 ~character:33
  and stop = Position.create ~line:4 ~character:36 in
  Util.test ~start ~stop source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 6, "line": 5 },
        "start": { "character": 0, "line": 1 }
      },
      "content": "let const_name1 = '@'\nlet f () : (module EMPTY) =\n  (module struct\n    let const_name2 = assert false\n    let secret = String.make 100 const_name1\n  end)",
      "selection_range": {
        "end": { "character": 15, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    } |}]
;;

let%expect_test "Example sample from merlin 2" =
  let source =
    {|let fun_name1 () = ()
    
let all_empty l =
  List.for_all
    (function
      | [] -> true
      | _ -> false)
    l
|}
  in
  let start = Position.create ~line:4 ~character:4
  and stop = Position.create ~line:6 ~character:19 in
  Util.test ~start ~stop source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 5, "line": 7 },
        "start": { "character": 0, "line": 2 }
      },
      "content": "let fun_name2 = (function | [] -> true | _ -> false)\nlet all_empty l =\n  List.for_all\n    fun_name2 \n    l",
      "selection_range": {
        "end": { "character": 13, "line": 2 },
        "start": { "character": 4, "line": 2 }
      }
    } |}]
;;

let%expect_test "Example sample from merlin 3" =
  let source =
    {|(* A comment *)
let z = "..."

let test x y =
  let fun_name2 = Fun.id in
  let m =
    let m = print_endline (x ^ y ^ z) in
    m
  in
  m
|}
  in
  let start = Position.create ~line:6 ~character:12
  and stop = Position.create ~line:6 ~character:37 in
  Util.test ~extract_name:"print_xyz" ~start ~stop source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 3, "line": 9 },
        "start": { "character": 0, "line": 3 }
      },
      "content": "let print_xyz (x) (y) = print_endline (x ^ (y ^ z))\nlet test x y =\n  let fun_name2 = Fun.id in\n  let m =\n    let m = print_xyz x y in\n    m\n  in\n  m",
      "selection_range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "Example sample from merlin 4" =
  let source =
    {|let f =
  print_endline "Wild side effect!";
  1 :: [ 2; 3; 4 ] 
|}
  in
  let start = Position.create ~line:1 ~character:12
  and stop = Position.create ~line:1 ~character:37 in
  Util.test ~extract_name:"show" ~start ~stop source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 18, "line": 2 },
        "start": { "character": 0, "line": 0 }
      },
      "content": "let show = \"Wild side effect!\"\nlet f =\n  print_endline show;\n  1 :: [ 2; 3; 4 ]",
      "selection_range": {
        "end": { "character": 8, "line": 0 },
        "start": { "character": 4, "line": 0 }
      }
    }
    |}]
;;

let%expect_test "Example sample from merlin 5" =
  let source =
    {|class a =
  let inner_expr =
    let bar = 20 in
    object
      method foo = bar
    end
  in
  object
    method x = (Fun.const 10) ()
    method y = print_endline
    method z =
      let x =
        object
          method x = "foobar"
        end
      in
      x
  end

and b = object end
|}
  in
  let start = Position.create ~line:2 ~character:4
  and stop = Position.create ~line:5 ~character:37 in
  Util.test ~extract_name:"outsider_expr" ~start ~stop source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 5, "line": 17 },
        "start": { "character": 0, "line": 0 }
      },
      "content": "let outsider_expr () = let bar = 20 in object method foo = bar end\nclass a =\n  let inner_expr =\n    outsider_expr ()\n  in\n  object\n    method x = (Fun.const 10) ()\n    method y = print_endline\n    method z =\n      let x =\n        object\n          method x = \"foobar\"\n        end\n      in\n      x\n  end",
      "selection_range": {
        "end": { "character": 17, "line": 0 },
        "start": { "character": 4, "line": 0 }
      }
    }
    |}]
;;
