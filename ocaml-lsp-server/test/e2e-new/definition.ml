open Test.Import

let print_locations =
  Test.print_option (function
    | `Definition definition -> Definition.yojson_of_t definition
    | `DefinitionLink links -> `List (List.map links ~f:DefinitionLink.yojson_of_t))
;;

let definition client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (TextDocumentDefinition (DefinitionParams.create ~textDocument ~position ()))
;;

let print_definition_error label = function
  | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
    ->
    Printf.printf "%s: %s" label error.message;
    Option.iter error.data ~f:(fun data ->
      Printf.printf " (%s)" (Yojson.Safe.to_string data));
    print_newline ();
    Fiber.return ()
  | Error errors -> Fiber.reraise_all errors
  | Ok _ ->
    Printf.printf "%s unexpectedly succeeded\n" label;
    Fiber.return ()
;;

let%expect_test "reports definition lookup failures without stopping the server" =
  let source =
    "let origin = 1\n\
     let missing_use = missing\n\
     let sum = 1 + 2\n\
     let typed : int = 1\n\
     let use = origin\n"
  in
  let req client =
    let check label position =
      let* result = Fiber.collect_errors (fun () -> definition client position) in
      print_definition_error label result
    in
    let* () = check "at origin" (Position.create ~line:0 ~character:4) in
    let* () = check "missing" (Position.create ~line:1 ~character:18) in
    let* () = check "builtin" (Position.create ~line:3 ~character:12) in
    let* response = definition client (Position.create ~line:4 ~character:10) in
    Printf.printf "server remains usable: %b\n" (Option.is_some response);
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    at origin: Request "Jump to definition" failed. ("Locate: Already at definition point")
    missing: Request "Jump to definition" failed. ("Locate: Not in environment: missing")
    builtin: Request "Jump to definition" failed. ("Locate: \"int\" is a builtin, it is not possible to jump to its definition")
    server remains usable: true
    |}]
;;

let%expect_test "returns location of a definition" =
  let source =
    {ocaml|let x = 43

let () =
  print_int x
|ocaml}
  in
  let req client =
    let* response = definition client (Position.create ~line:3 ~character:12) in
    print_locations response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 4, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///test.ml"
      }
    ]
    |}]
;;
