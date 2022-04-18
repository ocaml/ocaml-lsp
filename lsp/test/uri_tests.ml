open Lsp

let run_with_modes f =
  print_endline "Unix:";
  Lsp.Uri.Private.win32 := false;
  f ();
  print_endline "Windows:";
  Lsp.Uri.Private.win32 := true;
  f ()

let test_uri_parsing =
  let test s =
    let uri = Uri.t_of_yojson (`String s) in
    Printf.printf "%s -> %s\n" s (Uri.to_path uri)
  in
  fun uris -> run_with_modes (fun () -> List.iter test uris)

let%expect_test "test uri parsing" =
  test_uri_parsing [ "file:///Users/foo"; "file:///c:/Users/foo" ];
  [%expect
    {|
    Unix:
    file:///Users/foo -> /Users/foo
    file:///c:/Users/foo -> /c:/Users/foo
    Windows:
    file:///Users/foo -> Users/foo
    file:///c:/Users/foo -> c:/Users/foo |}]

let uri_of_path =
  let test path =
    let uri = Uri.of_path path in
    Printf.printf "%s -> %s\n" path (Uri.to_string uri)
  in
  fun uris -> run_with_modes (fun () -> List.iter test uris)

let%expect_test "uri of path" =
  uri_of_path [ "/foo/bar.ml"; "foo/bar.mli" ];
  [%expect
    {|
    Unix:
    /foo/bar.ml -> file:///foo/bar.ml
    foo/bar.mli -> file:///foo/bar.mli
    Windows:
    /foo/bar.ml -> file:///foo/bar.ml
    foo/bar.mli -> file:///foo/bar.mli |}]
