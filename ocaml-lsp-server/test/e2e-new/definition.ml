open Test.Import

let print_locations = Test.print_option Locations.yojson_of_t

let rec censor_backtraces = function
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (name, value) ->
         if String.equal name "backtrace"
         then name, `String "<censored>"
         else name, censor_backtraces value))
  | `List values -> `List (List.map values ~f:censor_backtraces)
  | json -> json
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

let%expect_test "definition of a nullary exception leaks Not_found" =
  let source = "exception E\nE" in
  Helpers.test source (fun client ->
    let* result =
      Fiber.collect_errors (fun () ->
        definition client (Position.create ~line:1 ~character:1))
    in
    match result with
    | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
      ->
      Jsonrpc.Response.Error.yojson_of_t error |> censor_backtraces |> Test.print_result;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors
    | Ok response ->
      print_locations response;
      Fiber.return ());
  [%expect
    {|
    {
      "data": { "exn": "Not_found", "backtrace": "<censored>" },
      "code": -32603,
      "message": "uncaught exception"
    }
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
