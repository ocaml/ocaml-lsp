open Lsp

let%expect_test "test uri parsing" =
  let uri = Uri.t_of_yojson (`String "file:///Users/foo") in
  print_endline (Uri.to_path uri);
  [%expect {|
    /Users/foo |}];
  print_endline (Uri.to_string uri);
  [%expect {| file:///Users/foo |}];
  let uri = Uri.t_of_yojson (`String "file:///c:/Users/foo") in
  print_endline (Uri.to_path uri);
  [%expect {| /c:/Users/foo |}];
  print_endline (Uri.to_string uri);
  [%expect {|
    file:///c:/Users/foo |}];
  let uri = Uri.t_of_yojson (`String "file:///n%C3%A9w/lib%20copy%281%29.ml") in
  print_endline (Uri.to_path uri);
  [%expect {| /néw/lib copy(1).ml |}]

let%expect_test "uri of path" =
  let uri = Uri.of_path "/foo/bar.ml" in
  print_endline (Uri.to_string uri);
  [%expect {|
    file:///foo/bar.ml |}];
  let uri = Uri.of_path "foo/bar.mli" in
  print_endline (Uri.to_string uri);
  [%expect {|
    file:///foo/bar.mli |}]
