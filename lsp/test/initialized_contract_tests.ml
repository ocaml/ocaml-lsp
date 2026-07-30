open Lsp

let print_json json = Yojson.Safe.pretty_to_string json |> Stdlib.print_endline

let%expect_test "initialized notification params" =
  let encoded = Client_notification.to_jsonrpc Client_notification.Initialized in
  (match encoded.params with
   | None -> Stdlib.print_endline "encoded params: omitted"
   | Some params -> print_json (params :> Yojson.Safe.t));
  let check label params =
    Jsonrpc.Notification.create ~method_:"initialized" ~params ()
    |> Client_notification.of_jsonrpc
    |> function
    | Ok _ -> Stdlib.Printf.printf "%s: accepted\n" label
    | Error _ -> Stdlib.Printf.printf "%s: rejected\n" label
  in
  check "object params" (`Assoc []);
  check "array params" (`List []);
  [%expect
    {|
    encoded params: omitted
    object params: accepted
    array params: accepted
    |}]
;;
