open Lsp

let%expect_test "test uri parsing" =
  let uri = Uri.t_of_yojson (`String "file:///Users/foo") in
  print_endline (Uri.to_path uri);
  [%expect {|
    /Users/foo |}]
