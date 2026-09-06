open Lsp
open Types

let print_work_done { ProgressParams.token; value } =
  let kind =
    match value with
    | Progress.Begin _ -> "begin"
    | Report _ -> "report"
    | End _ -> "end"
  in
  Printf.printf
    "WorkDoneProgress/%s token=%s value=%s\n"
    kind
    (Yojson.Safe.to_string (ProgressToken.yojson_of_t token))
    (Yojson.Safe.to_string (Progress.yojson_of_t value))
;;

let check label ?params () =
  print_endline label;
  let notification = Jsonrpc.Notification.create ~method_:"$/progress" ?params () in
  let print_result direction print_decoded to_jsonrpc = function
    | Error _ -> Printf.printf "%s: rejected\n" direction
    | Ok decoded ->
      Printf.printf "%s: " direction;
      print_decoded decoded;
      let encoded = Jsonrpc.Notification.yojson_of_t (to_jsonrpc decoded) in
      Printf.printf
        "round-trip preserved: %b\n"
        (Yojson.Safe.equal (Jsonrpc.Notification.yojson_of_t notification) encoded)
  in
  print_result
    "client to server"
    (function
      | Client_notification.WorkDoneProgress params -> print_work_done params
      | _ -> failwith "expected a progress notification")
    Client_notification.to_jsonrpc
    (Client_notification.of_jsonrpc notification);
  print_result
    "server to client"
    (function
      | Server_notification.WorkDoneProgress params -> print_work_done params
      | _ -> failwith "expected a progress notification")
    Server_notification.to_jsonrpc
    (Server_notification.of_jsonrpc notification)
;;

let check_progress label token value =
  (* Put value first to check that round-trip comparisons ignore object key order. *)
  check label ~params:(`Assoc [ "value", value; "token", token ]) ()
;;

let%expect_test "generic progress notifications" =
  let position = Position.create ~line:2 ~character:4 in
  let location =
    Location.create
      ~uri:(DocumentUri.of_string "file:///workspace/test.ml")
      ~range:(Range.create ~start:position ~end_:position)
  in
  check_progress
    "partial reference results"
    (`String "partial")
    (`List [ Location.yojson_of_t location ]);
  [%expect
    {|
    partial reference results
    client to server: rejected
    server to client: rejected
    |}]
;;

let%expect_test "generic progress values with integer tokens" =
  List.iter
    (fun (label, value) -> check_progress label (`Int 42) value)
    [ "empty array", `List []
    ; ( "semantic token chunk"
      , `Assoc [ "data", `List [ `Int 0; `Int 0; `Int 3; `Int 0; `Int 0 ] ] )
    ; ( "nested object"
      , `Assoc [ "items", `List [ `Assoc [ "id", `Int 1; "metadata", `Null ] ] ] )
    ; "empty object", `Assoc []
    ; "string", `String "chunk"
    ; "integer", `Int 17
    ; "decimal", `Float 1.5
    ; "boolean", `Bool true
    ; "null", `Null
    ];
  [%expect
    {|
    empty array
    client to server: rejected
    server to client: rejected
    semantic token chunk
    client to server: rejected
    server to client: rejected
    nested object
    client to server: rejected
    server to client: rejected
    empty object
    client to server: rejected
    server to client: rejected
    string
    client to server: rejected
    server to client: rejected
    integer
    client to server: rejected
    server to client: rejected
    decimal
    client to server: rejected
    server to client: rejected
    boolean
    client to server: rejected
    server to client: rejected
    null
    client to server: rejected
    server to client: rejected
    |}]
;;

let%expect_test "work-done notifications preserve typed variants and payloads" =
  List.iter
    (fun (label, token, value) ->
       check_progress label token (Yojson.Safe.from_string value))
    [ ( "begin"
      , `String "build"
      , {|{"title":"Build","kind":"begin","percentage":0,"message":"started","cancellable":true}|}
      )
    ; ( "report"
      , `Int 42
      , {|{"message":"Building [3/10]","kind":"report","percentage":30,"cancellable":false}|}
      )
    ; "end", `String "build", {|{"message":"Build finished","kind":"end"}|}
    ; "minimal begin", `Int 0, {|{"title":"Build","kind":"begin"}|}
    ; "minimal report", `String "", {|{"kind":"report"}|}
    ; "minimal end", `Int 42, {|{"kind":"end"}|}
    ];
  [%expect
    {|
    begin
    client to server: WorkDoneProgress/begin token="build" value={"kind":"begin","cancellable":true,"message":"started","percentage":0,"title":"Build"}
    round-trip preserved: true
    server to client: WorkDoneProgress/begin token="build" value={"kind":"begin","cancellable":true,"message":"started","percentage":0,"title":"Build"}
    round-trip preserved: true
    report
    client to server: WorkDoneProgress/report token=42 value={"kind":"report","cancellable":false,"message":"Building [3/10]","percentage":30}
    round-trip preserved: true
    server to client: WorkDoneProgress/report token=42 value={"kind":"report","cancellable":false,"message":"Building [3/10]","percentage":30}
    round-trip preserved: true
    end
    client to server: WorkDoneProgress/end token="build" value={"kind":"end","message":"Build finished"}
    round-trip preserved: true
    server to client: WorkDoneProgress/end token="build" value={"kind":"end","message":"Build finished"}
    round-trip preserved: true
    minimal begin
    client to server: WorkDoneProgress/begin token=0 value={"kind":"begin","title":"Build"}
    round-trip preserved: true
    server to client: WorkDoneProgress/begin token=0 value={"kind":"begin","title":"Build"}
    round-trip preserved: true
    minimal report
    client to server: WorkDoneProgress/report token="" value={"kind":"report"}
    round-trip preserved: true
    server to client: WorkDoneProgress/report token="" value={"kind":"report"}
    round-trip preserved: true
    minimal end
    client to server: WorkDoneProgress/end token=42 value={"kind":"end"}
    round-trip preserved: true
    server to client: WorkDoneProgress/end token=42 value={"kind":"end"}
    round-trip preserved: true
    |}]
;;

let%expect_test "work-done-looking generic values expose rejection and data loss" =
  (* A generic payload may use work-done field names with unrelated meanings. *)
  List.iter
    (fun (label, value) ->
       check_progress label (`String "partial") (Yojson.Safe.from_string value))
    [ "unknown kind", {|{"kind":"chunk","items":[1]}|}
    ; "non-string kind", {|{"kind":7,"items":[1]}|}
    ; "missing work-done field", {|{"kind":"begin"}|}
    ; "wrong work-done field type", {|{"kind":"report","percentage":"half"}|}
    ; "extra begin field", {|{"kind":"begin","title":"Chunk","items":[1]}|}
    ; "extra report field", {|{"kind":"report","items":[1]}|}
    ; "extra end field", {|{"kind":"end","message":"done","items":[1]}|}
    ; "null optional field", {|{"kind":"report","message":null}|}
    ];
  [%expect
    {|
    unknown kind
    client to server: rejected
    server to client: rejected
    non-string kind
    client to server: rejected
    server to client: rejected
    missing work-done field
    client to server: rejected
    server to client: rejected
    wrong work-done field type
    client to server: rejected
    server to client: rejected
    extra begin field
    client to server: WorkDoneProgress/begin token="partial" value={"kind":"begin","title":"Chunk"}
    round-trip preserved: false
    server to client: WorkDoneProgress/begin token="partial" value={"kind":"begin","title":"Chunk"}
    round-trip preserved: false
    extra report field
    client to server: WorkDoneProgress/report token="partial" value={"kind":"report"}
    round-trip preserved: false
    server to client: WorkDoneProgress/report token="partial" value={"kind":"report"}
    round-trip preserved: false
    extra end field
    client to server: WorkDoneProgress/end token="partial" value={"kind":"end","message":"done"}
    round-trip preserved: false
    server to client: WorkDoneProgress/end token="partial" value={"kind":"end","message":"done"}
    round-trip preserved: false
    null optional field
    client to server: WorkDoneProgress/report token="partial" value={"kind":"report"}
    round-trip preserved: false
    server to client: WorkDoneProgress/report token="partial" value={"kind":"report"}
    round-trip preserved: false
    |}]
;;

let%expect_test "malformed progress envelopes remain rejected" =
  let value = `Assoc [ "kind", `String "end" ] in
  check "missing params" ();
  check "array params" ~params:(`List []) ();
  check "empty params" ~params:(`Assoc []) ();
  check "missing token" ~params:(`Assoc [ "value", value ]) ();
  check "missing value" ~params:(`Assoc [ "token", `String "build" ]) ();
  List.iter
    (fun (label, token) -> check_progress label token value)
    [ "null token", `Null
    ; "boolean token", `Bool true
    ; "decimal token", `Float 1.5
    ; "array token", `List []
    ; "object token", `Assoc []
    ];
  check
    "duplicate token"
    ~params:(`Assoc [ "token", `Int 1; "token", `Int 2; "value", value ])
    ();
  check
    "duplicate value"
    ~params:(`Assoc [ "token", `Int 1; "value", value; "value", value ])
    ();
  [%expect
    {|
    missing params
    client to server: rejected
    server to client: rejected
    array params
    client to server: rejected
    server to client: rejected
    empty params
    client to server: rejected
    server to client: rejected
    missing token
    client to server: rejected
    server to client: rejected
    missing value
    client to server: rejected
    server to client: rejected
    null token
    client to server: rejected
    server to client: rejected
    boolean token
    client to server: rejected
    server to client: rejected
    decimal token
    client to server: rejected
    server to client: rejected
    array token
    client to server: rejected
    server to client: rejected
    object token
    client to server: rejected
    server to client: rejected
    duplicate token
    client to server: rejected
    server to client: rejected
    duplicate value
    client to server: rejected
    server to client: rejected
    |}]
;;
