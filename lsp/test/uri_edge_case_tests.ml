open Lsp

(* Characterization tests for deferred fixes, not promises that these quirks are
   desirable. The inputs are codec probes, not observed client traffic. *)
let print_uri uri =
  let serialized = Uri.to_string uri in
  assert (uri = Uri.of_string serialized);
  let from_json =
    Uri.yojson_of_t uri
    |> Yojson.Safe.to_string
    |> Yojson.Safe.from_string
    |> Uri.t_of_yojson
  in
  assert (uri = from_json);
  Printf.printf "  uri: %s\n  path: %S\n" serialized (Uri.to_path uri)
;;

let parse_uri source =
  let uri = Uri.of_string source in
  assert (uri = Uri.t_of_yojson (`String source));
  print_endline source;
  print_uri uri;
  uri
;;

let%expect_test "POSIX drive-like paths: current behavior" =
  (* FIXME: drive interpretation changes case and drops the leading slash even
     on POSIX. These are valid, distinct POSIX pathnames. *)
  Uri.For_tests.with_win32 false (fun () ->
    List.iter
      (fun path ->
         Printf.printf "of_path %S\n" path;
         print_uri (Uri.of_path path))
      [ "/C:/project/a.ml"; "/c:/project/a.ml" ];
    Printf.printf
      "equal keys: %b\n"
      (Uri.equal (Uri.of_path "/C:/project/a.ml") (Uri.of_path "/c:/project/a.ml")));
  [%expect
    {|
    of_path "/C:/project/a.ml"
      uri: file:///c%3A/project/a.ml
      path: "c:/project/a.ml"
    of_path "/c:/project/a.ml"
      uri: file:///c%3A/project/a.ml
      path: "c:/project/a.ml"
    equal keys: true
    |}]
;;

let%expect_test "Windows file URI aliases: current behavior" =
  (* FIXME: these aliases can produce different URI keys after reconstruction
     from the Windows path. A literal %5C filename sequence is a control. *)
  Uri.For_tests.with_win32 true (fun () ->
    List.iter
      (fun source ->
         let uri = parse_uri source in
         let reconstructed = Uri.of_path (Uri.to_path uri) in
         Printf.printf
           "  reconstructed: %s\n  same key: %b\n"
           (Uri.to_string reconstructed)
           (Uri.equal uri reconstructed))
      [ "file:////server/share/a.ml"
      ; "file://server/share%5Ca.ml"
      ; "file:///C:/dir%5Ca.ml"
      ; "file:///C:/dir%255Ca.ml"
      ]);
  [%expect
    {|
    file:////server/share/a.ml
      uri: file:////server/share/a.ml
      path: "\\\\server\\share\\a.ml"
      reconstructed: file://server/share/a.ml
      same key: false
    file://server/share%5Ca.ml
      uri: file://server/share%5Ca.ml
      path: "\\\\server\\share\\a.ml"
      reconstructed: file://server/share/a.ml
      same key: false
    file:///C:/dir%5Ca.ml
      uri: file:///c%3A/dir%5Ca.ml
      path: "c:\\dir\\a.ml"
      reconstructed: file:///c%3A/dir/a.ml
      same key: false
    file:///C:/dir%255Ca.ml
      uri: file:///c%3A/dir%255Ca.ml
      path: "c:\\dir%5Ca.ml"
      reconstructed: file:///c%3A/dir%255Ca.ml
      same key: true
    |}]
;;

let%expect_test "file IP-literal authorities: current behavior" =
  (* FIXME: file hosts are encoded as data, losing IP-literal syntax and its
     distinction from escaped registered-name data. Zone case is also lost. *)
  Uri.For_tests.with_win32 false (fun () ->
    let literal = parse_uri "file://[::1]/share/a.ml" in
    let escaped = parse_uri "file://%5B%3A%3A1%5D/share/a.ml" in
    Printf.printf "literal = escaped registered name: %b\n" (Uri.equal literal escaped);
    List.iter
      (fun source -> ignore (parse_uri source))
      [ "file://[2001:DB8::A]/share/a.ml"; "file://[FE80::1%25ETH0]/share/a.ml" ]);
  [%expect
    {|
    file://[::1]/share/a.ml
      uri: file://%5B%3A%3A1%5D/share/a.ml
      path: "//[::1]/share/a.ml"
    file://%5B%3A%3A1%5D/share/a.ml
      uri: file://%5B%3A%3A1%5D/share/a.ml
      path: "//[::1]/share/a.ml"
    literal = escaped registered name: true
    file://[2001:DB8::A]/share/a.ml
      uri: file://%5B2001%3Adb8%3A%3Aa%5D/share/a.ml
      path: "//[2001:db8::a]/share/a.ml"
    file://[FE80::1%25ETH0]/share/a.ml
      uri: file://%5Bfe80%3A%3A1%25eth0%5D/share/a.ml
      path: "//[fe80::1%eth0]/share/a.ml"
    |}]
;;

let%expect_test "localhost file authorities: current behavior" =
  (* FIXME: RFC 8089 treats localhost as absent authority, unlike the current
     UNC interpretation. Any change must account for native Windows UNC shares. *)
  List.iter
    (fun windows ->
       Uri.For_tests.with_win32 windows (fun () ->
         print_endline (if windows then "Windows:" else "Unix:");
         let uri = parse_uri "file://localhost/tmp/a.ml" in
         Printf.printf
           "same as local URI: %b\n"
           (Uri.equal uri (Uri.of_string "file:///tmp/a.ml"))))
    [ false; true ];
  Uri.For_tests.with_win32 true (fun () ->
    print_endline "Native localhost UNC share:";
    print_uri (Uri.of_path "\\\\localhost\\Share\\a.ml"));
  [%expect
    {|
    Unix:
    file://localhost/tmp/a.ml
      uri: file://localhost/tmp/a.ml
      path: "//localhost/tmp/a.ml"
    same as local URI: false
    Windows:
    file://localhost/tmp/a.ml
      uri: file://localhost/tmp/a.ml
      path: "\\\\localhost\\tmp\\a.ml"
    same as local URI: false
    Native localhost UNC share:
      uri: file://localhost/Share/a.ml
      path: "\\\\localhost\\Share\\a.ml"
    |}]
;;

let%expect_test "remote file roots: current behavior" =
  (* FIXME: a host-only root silently becomes the local root, unlike a path
     with a share component. Root slash and suffix presence do not prevent it. *)
  List.iter
    (fun windows ->
       Uri.For_tests.with_win32 windows (fun () ->
         print_endline (if windows then "Windows:" else "Unix:");
         List.iter
           (fun source -> ignore (parse_uri source))
           [ "file://server"
           ; "file://server/"
           ; "file://server/?q=%26#"
           ; "file://server/share"
           ; "file:///"
           ]))
    [ false; true ];
  [%expect
    {|
    Unix:
    file://server
      uri: file://server/
      path: "/"
    file://server/
      uri: file://server/
      path: "/"
    file://server/?q=%26#
      uri: file://server/?q=%26#
      path: "/"
    file://server/share
      uri: file://server/share
      path: "//server/share"
    file:///
      uri: file:///
      path: "/"
    Windows:
    file://server
      uri: file://server/
      path: "\\"
    file://server/
      uri: file://server/
      path: "\\"
    file://server/?q=%26#
      uri: file://server/?q=%26#
      path: "\\"
    file://server/share
      uri: file://server/share
      path: "\\\\server\\share"
    file:///
      uri: file:///
      path: "\\"
    |}]
;;
