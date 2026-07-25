let check_round_trip packet =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let reparsed = Jsonrpc.Packet.t_of_yojson json in
  if packet <> reparsed
  then
    failwith
      (Printf.sprintf
         "packet did not round trip:\n%s"
         (Yojson.Safe.pretty_to_string ~std:false json))
;;

let%expect_test "JSON-RPC packets round trip through JSON" =
  let request id method_ =
    Jsonrpc.Request.create
      ~id
      ~method_
      ~params:(`Assoc [ "argument", `String "😀" ])
      ()
  in
  let notification =
    Jsonrpc.Notification.create
      ~method_:"notify"
      ~params:(`List [ `Int 1; `Bool true ])
      ()
  in
  let response = Jsonrpc.Response.ok (`Int 1) (`Assoc [ "answer", `Int 42 ]) in
  let error =
    Jsonrpc.Response.Error.make
      ~code:InvalidParams
      ~message:"bad parameters"
      ~data:(`List [ `String "details" ])
      ()
  in
  [ Jsonrpc.Packet.Request (request (`Int 1) "request/int")
  ; Request (request (`String "two") "request/string")
  ; Notification notification
  ; Response response
  ; Response (Jsonrpc.Response.error (`String "error") error)
  ; Batch_call
      [ `Request (request (`Int 3) "batch/request"); `Notification notification ]
  ; Batch_response
      [ response; Jsonrpc.Response.error (`String "batch-error") error ]
  ]
  |> List.iter check_round_trip;
  [%expect {| |}]
;;
