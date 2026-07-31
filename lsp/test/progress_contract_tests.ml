open Lsp
open Types

let%expect_test "generic progress notifications" =
  let position = Position.create ~line:2 ~character:4 in
  let location =
    Location.create
      ~uri:(DocumentUri.of_string "file:///workspace/test.ml")
      ~range:(Range.create ~start:position ~end_:position)
  in
  let params =
    `Assoc
      [ "token", `String "partial"; "value", `List [ Location.yojson_of_t location ] ]
  in
  let notification = Jsonrpc.Notification.create ~method_:"$/progress" ~params () in
  let print_result label = function
    | Ok _ -> Stdlib.Printf.printf "%s: accepted\n" label
    | Error _ -> Stdlib.Printf.printf "%s: rejected\n" label
  in
  print_result "client to server" (Client_notification.of_jsonrpc notification);
  print_result "server to client" (Server_notification.of_jsonrpc notification);
  [%expect
    {|
    client to server: rejected
    server to client: rejected
    |}]
;;
