open Lsp

let run_with_modes f =
  print_endline "Unix:";
  Lsp.Uri.Private.win32 := false;
  f ();
  print_endline "Windows:";
  Lsp.Uri.Private.win32 := true;
  f ()
;;

let test_uri_parsing =
  let test s =
    let uri = Uri.t_of_yojson (`String s) in
    Printf.printf "%s -> %s\n" s (Uri.to_path uri);
    match Uri.query uri with
    | None -> ()
    | Some q -> Printf.printf "query: %s\n" q
  in
  fun uris -> run_with_modes (fun () -> List.iter test uris)
;;

let%expect_test "test uri parsing" =
  test_uri_parsing
    [ "file:///Users/foo"
    ; "file:///c:/Users/foo"
    ; "file:///foo?x=y"
    ; "http://xyz?foo#"
    ; "http://xxx?"
    ; "http://xyz?ab%3D1%23"
    ];
  [%expect
    {|
    Unix:
    file:///Users/foo -> /Users/foo
    file:///c:/Users/foo -> c:/Users/foo
    file:///foo?x=y -> /foo
    query: x=y
    http://xyz?foo# -> /
    query: foo
    http://xxx? -> /
    query:
    http://xyz?ab%3D1%23 -> /
    query: ab=1#
    Windows:
    file:///Users/foo -> \Users\foo
    file:///c:/Users/foo -> c:\Users\foo
    file:///foo?x=y -> \foo
    query: x=y
    http://xyz?foo# -> \
    query: foo
    http://xxx? -> \
    query:
    http://xyz?ab%3D1%23 -> \
    query: ab=1# |}]
;;

let%expect_test "a URI query is not part of its filesystem path" =
  Lsp.Uri.Private.win32 := false;
  let uri = Uri.of_string "file:///foo.ml?revision=1" in
  Printf.printf
    "path: %s\nquery: %s\n"
    (Uri.to_path uri)
    (Option.value ~default:"<none>" (Uri.query uri));
  [%expect
    {|
    path: /foo.ml
    query: revision=1
    |}]
;;

let%expect_test "serialization disambiguates a path beginning with two slashes" =
  let uri = Uri.of_string "untitled:///%2FModule.ml" in
  let serialized = Uri.to_string uri in
  let round_trip = Uri.of_string serialized in
  Printf.printf
    "original: %s\nserialized: %s\nparsed serialization: %s\n"
    (Uri.to_string uri)
    serialized
    (Uri.to_string round_trip);
  [%expect
    {|
    original: untitled:///%2FModule.ml
    serialized: untitled:///%2FModule.ml
    parsed serialization: untitled:///%2FModule.ml
    |}]
;;

let%expect_test "a percent-encoded URI fragment round trips" =
  let uri = Uri.of_string "file:///foo.ml#heading%201" in
  Printf.printf
    "fragment: %s\nserialized: %s\n"
    (Option.value ~default:"<none>" (Uri.fragment uri))
    (Uri.to_string uri);
  [%expect
    {|
    fragment: heading 1
    serialized: file:///foo.ml#heading%201
    |}]
;;

let%expect_test "serialization preserves a non-letter drive-like path" =
  let source = "file:///1%3A/foo.ml" in
  let serialized = Uri.of_string source |> Uri.to_string in
  Printf.printf "source: %s\nserialized: %s\n" source serialized;
  [%expect
    {|
    source: file:///1%3A/foo.ml
    serialized: file:///1%3A/foo.ml
    |}]
;;

let%expect_test "JSON file URI serialization normalizes path escapes" =
  let encoded_slash = `String "file:///pro%2Fjects/test.ml" in
  let literal_slash = `String "file:///pro/jects/test.ml" in
  let print_normalized label input =
    let normalized = Uri.t_of_yojson input |> Uri.yojson_of_t in
    Printf.printf
      "%s: %s -> %s\n"
      label
      (Yojson.Safe.to_string input)
      (Yojson.Safe.to_string normalized)
  in
  print_normalized "encoded slash" encoded_slash;
  print_normalized "literal slash" literal_slash;
  [%expect
    {|
    encoded slash: "file:///pro%2Fjects/test.ml" -> "file:///pro/jects/test.ml"
    literal slash: "file:///pro/jects/test.ml" -> "file:///pro/jects/test.ml"
    |}]
;;

let%expect_test "query separators and escaped values" =
  List.iter
    (fun source ->
       let serialized = Uri.of_string source |> Uri.to_string in
       assert (Uri.yojson_of_t (Uri.t_of_yojson (`String source)) = `String serialized);
       Printf.printf "%s -> %s\n" source serialized)
    [ "https://ocaml.org/search?q=a+b&page=1"
    ; "https://example.org/?q=%26"
    ; "https://example.org/?q=a%2Bb"
    ; "https://example.org/?q=a%26admin%3Dtrue"
    ; "https://ocaml.org/?q=%23tag"
    ; "file:///foo.ml#L3,4"
    ];
  [%expect
    {|
    https://ocaml.org/search?q=a+b&page=1 -> https://ocaml.org/search?q=a+b&page=1
    https://example.org/?q=%26 -> https://example.org/?q=%26
    https://example.org/?q=a%2Bb -> https://example.org/?q=a%2Bb
    https://example.org/?q=a%26admin%3Dtrue -> https://example.org/?q=a%26admin%3Dtrue
    https://ocaml.org/?q=%23tag -> https://ocaml.org/?q=%23tag
    file:///foo.ml#L3,4 -> file:///foo.ml#L3,4
    |}]
;;

let%expect_test "normalization preserves meaningful component boundaries" =
  List.iter
    (fun (source, expected) ->
       let uri = Uri.of_string source in
       assert (String.equal expected (Uri.to_string uri));
       assert (Uri.yojson_of_t uri = `String expected))
    [ "", ""
    ; "/", "/"
    ; "//", "//"
    ; "//host", "//host"
    ; "//host/", "//host/"
    ; "?", "?"
    ; "#", "#"
    ; "?#", "?#"
    ; "relative/path?query#fragment", "relative/path?query#fragment"
    ; "file:", "file:///"
    ; "file:/", "file:///"
    ; "file://", "file:///"
    ; "file:///", "file:///"
    ; "file:relative", "file:///relative"
    ; "x:opaque", "x:opaque"
    ; "x:/path", "x:/path"
    ; "x:///path", "x:///path"
    ; "x://?#", "x://?#"
    ; ( "https://user:p%40ss@HOST:8042/a%2fb?q=a%26b&k=%2B#x%23y"
      , "https://user:p%40ss@HOST:8042/a%2Fb?q=a%26b&k=%2B#x%23y" )
    ];
  [%expect {| |}]
;;

let%expect_test "decoded views neither decode twice nor alter URI syntax" =
  let source = "file:///a%252Fb.ml?q=a%2526b#p%2523q" in
  let uri = Uri.of_string source in
  assert (Uri.query uri = Some "q=a%26b");
  assert (Uri.fragment uri = Some "p%23q");
  assert (String.equal (Uri.to_string uri) source);
  [%expect {| |}]
;;

let%expect_test "normalization cannot introduce escapes from malformed percent data" =
  let source = "x:/%%36%31?q=%2%36#%GG" in
  let uri = Uri.of_string source in
  assert (String.equal (Uri.to_string uri) "x:/%2561?q=%2526#%25GG");
  assert (Uri.query uri = Some "q=%26");
  assert (Uri.fragment uri = Some "%GG");
  assert (Uri.of_string (Uri.to_string uri) = uri);
  [%expect {| |}]
;;

let%expect_test "URI identity preserves meaningful escaping" =
  List.iter
    (fun (left, right) ->
       let left = Uri.of_string left in
       let right = Uri.of_string right in
       assert (not (Uri.equal left right));
       assert (Uri.compare left right <> 0))
    [ "https://example.org/?q=a%26b", "https://example.org/?q=a&b"
    ; "file:///foo.ml?q=a%26b", "file:///foo.ml?q=a&b"
    ; "https://example.org/?q=a%2Bb", "https://example.org/?q=a+b"
    ; "https://example.org/a%2Fb", "https://example.org/a/b"
    ; "untitled:///%2FModule.ml", "untitled:////Module.ml"
    ; "x://user%40host/path", "x://user@host/path"
    ; "file:///foo.ml", "file:///foo.ml?"
    ; "file:///foo.ml", "file:///foo.ml#"
    ; "file://server/C:/foo.ml", "file://server/c:/foo.ml"
    ; "x:/path", "x:///path"
    ];
  [%expect {| |}]
;;

let%expect_test "equivalent URI components have consistent comparison and hashing" =
  List.iter
    (fun (left, right) ->
       let left = Uri.of_string left in
       let right = Uri.of_string right in
       assert (left = right);
       assert (Uri.equal left right);
       assert (Uri.compare left right = 0);
       assert (Uri.hash left = Uri.hash right);
       assert (String.equal (Uri.to_string left) (Uri.to_string right)))
    [ "https://example.org/%7e?q=%61", "HTTPS://example.org/~?q=a"
    ; "https://example.org/a%2fb", "https://example.org/a%2Fb"
    ; "file:///foo.ml", "file:/%66oo.ml"
    ; "file:///a%2fb/foo.ml", "file:///a/b/foo.ml"
    ; "file:///foo+bar.ml", "file:///foo%2bbar.ml"
    ; "file:///Mödel.ml", "file:///M%C3%B6del.ml"
    ; "file:///C:/foo.ml", "file:///c%3a/foo.ml"
    ; "file://SERVER/share/foo.ml", "file://server/share/foo.ml"
    ];
  [%expect {| |}]
;;

let%expect_test "all URI constructors produce the same normalized representation" =
  let previous = !Uri.Private.win32 in
  Fun.protect
    ~finally:(fun () -> Uri.Private.win32 := previous)
    (fun () ->
       List.iter
         (fun windows ->
            Uri.Private.win32 := windows;
            List.iter
              (fun (path, source, expected) ->
                 let from_path = Uri.of_path path in
                 List.iter
                   (fun uri ->
                      assert (uri = from_path);
                      assert (String.equal (Uri.to_string uri) expected);
                      assert (Uri.yojson_of_t uri = `String expected))
                   [ from_path; Uri.of_string source; Uri.t_of_yojson (`String source) ])
              [ "/tmp/probe.ml", "FILE:/tmp/%70robe.ml", "file:///tmp/probe.ml"
              ; "/tmp/a%26b.ml", "file:///tmp/a%2526b.ml", "file:///tmp/a%2526b.ml"
              ; "/tmp/a&b.ml", "file:/tmp/a&b.ml", "file:///tmp/a%26b.ml"
              ; ( (if windows then "C:\\tmp\\probe.ml" else "/C:/tmp/probe.ml")
                , "FILE:/C%3a/tmp/probe.ml"
                , "file:///c%3A/tmp/probe.ml" )
              ; ( "//SeRvEr/share/probe.ml"
                , "file://SERVER/share/probe.ml"
                , "file://server/share/probe.ml" )
              ])
         [ false; true ]);
  [%expect {| |}]
;;

let%expect_test "an unescaped Unicode URI query is preserved" =
  let uri = Uri.of_string "file:///foo.ml?search=😀&limit=1" in
  Printf.printf
    "query: %s\nserialized: %s\n"
    (Option.value ~default:"<none>" (Uri.query uri))
    (Uri.to_string uri);
  [%expect
    {|
    query: search=😀&limit=1
    serialized: file:///foo.ml?search=😀&limit=1
    |}]
;;

let uri_of_path =
  let test path =
    let uri = Uri.of_path path in
    Printf.printf "%s -> %s\n" path (Uri.to_string uri)
  in
  fun uris -> run_with_modes (fun () -> List.iter test uris)
;;

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
;;

let%expect_test "of_path -> to_string" =
  let test_of_path_to_string =
    let test path =
      let uri = Uri.of_path path in
      Printf.printf "%s -> %s\n" path (Uri.to_string uri)
    in
    fun paths -> run_with_modes (fun () -> List.iter test paths)
  in
  test_of_path_to_string
    [ "c:/win/path"
    ; "C:/win/path"
    ; "c:/win/path/"
    ; "/c:/win/path"
    ; "c:\\win\\path"
    ; "c:\\win/path"
    ; "\\\\localhost\\c$\\GitDevelopment\\express"
    ; "c:\\test with %\\path"
    ; "c:\\test with %25\\path"
    ; "c:\\test with %25\\c#code"
    ; "\\\\shäres\\path\\c#\\plugin.json"
    ; "\\\\shares\\"
    ; "a.file"
    ; "/Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js"
    ];
  [%expect
    {|
    Unix:
    c:/win/path -> file:///c%3A/win/path
    C:/win/path -> file:///c%3A/win/path
    c:/win/path/ -> file:///c%3A/win/path/
    /c:/win/path -> file:///c%3A/win/path
    c:\win\path -> file:///c%3A%5Cwin%5Cpath
    c:\win/path -> file:///c%3A%5Cwin/path
    \\localhost\c$\GitDevelopment\express -> file:///%5C%5Clocalhost%5Cc%24%5CGitDevelopment%5Cexpress
    c:\test with %\path -> file:///c%3A%5Ctest%20with%20%25%5Cpath
    c:\test with %25\path -> file:///c%3A%5Ctest%20with%20%2525%5Cpath
    c:\test with %25\c#code -> file:///c%3A%5Ctest%20with%20%2525%5Cc%23code
    \\shäres\path\c#\plugin.json -> file:///%5C%5Csh%C3%A4res%5Cpath%5Cc%23%5Cplugin.json
    \\shares\ -> file:///%5C%5Cshares%5C
    a.file -> file:///a.file
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    Windows:
    c:/win/path -> file:///c%3A/win/path
    C:/win/path -> file:///c%3A/win/path
    c:/win/path/ -> file:///c%3A/win/path/
    /c:/win/path -> file:///c%3A/win/path
    c:\win\path -> file:///c%3A/win/path
    c:\win/path -> file:///c%3A/win/path
    \\localhost\c$\GitDevelopment\express -> file://localhost/c%24/GitDevelopment/express
    c:\test with %\path -> file:///c%3A/test%20with%20%25/path
    c:\test with %25\path -> file:///c%3A/test%20with%20%2525/path
    c:\test with %25\c#code -> file:///c%3A/test%20with%20%2525/c%23code
    \\shäres\path\c#\plugin.json -> file://sh%C3%A4res/path/c%23/plugin.json
    \\shares\ -> file://shares/
    a.file -> file:///a.file
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    |}]
;;

let%expect_test "of_path -> to_path" =
  let test_of_path_to_path =
    let test path =
      let uri = Uri.of_path path in
      Printf.printf "%s -> %s\n" path (Uri.to_path uri)
    in
    fun paths -> run_with_modes (fun () -> List.iter test paths)
  in
  test_of_path_to_path
    [ "c:/win/path"
    ; "c:/win/path/"
    ; "C:/win/path"
    ; "/c:/win/path"
    ; "./c/win/path"
    ; "c:\\win\\path"
    ; "c:\\win/path"
    ; "\\\\localhost\\c$\\GitDevelopment\\express"
    ; "\\\\shares"
    ; "\\\\shares\\"
    ; "\\\\shäres\\path\\c#\\plugin.json"
    ; "c:\\test with %\\path"
    ; "c:\\test with %25\\c#code"
    ; "hello"
    ];
  [%expect
    {|
    Unix:
    c:/win/path -> c:/win/path
    c:/win/path/ -> c:/win/path/
    C:/win/path -> c:/win/path
    /c:/win/path -> c:/win/path
    ./c/win/path -> /./c/win/path
    c:\win\path -> c:\win\path
    c:\win/path -> c:\win/path
    \\localhost\c$\GitDevelopment\express -> /\\localhost\c$\GitDevelopment\express
    \\shares -> /\\shares
    \\shares\ -> /\\shares\
    \\shäres\path\c#\plugin.json -> /\\shäres\path\c#\plugin.json
    c:\test with %\path -> c:\test with %\path
    c:\test with %25\c#code -> c:\test with %25\c#code
    hello -> /hello
    Windows:
    c:/win/path -> c:\win\path
    c:/win/path/ -> c:\win\path\
    C:/win/path -> c:\win\path
    /c:/win/path -> c:\win\path
    ./c/win/path -> \.\c\win\path
    c:\win\path -> c:\win\path
    c:\win/path -> c:\win\path
    \\localhost\c$\GitDevelopment\express -> \\localhost\c$\GitDevelopment\express
    \\shares -> \
    \\shares\ -> \
    \\shäres\path\c#\plugin.json -> \\shäres\path\c#\plugin.json
    c:\test with %\path -> c:\test with %\path
    c:\test with %25\c#code -> c:\test with %25\c#code
    hello -> \hello
    |}]
;;

let%expect_test "of_string -> to_path" =
  let test_of_string_to_path =
    let test s =
      let uri = Uri.t_of_yojson (`String s) in
      Printf.printf "%s -> %s\n" s (Uri.to_path uri)
    in
    fun s -> run_with_modes (fun () -> List.iter test s)
  in
  test_of_string_to_path
    [ "file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp"
    ; "file://shares/pröjects/c%23/#l12"
    ; "file:///_:/path"
    ; "file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins"
    ; "shares/pröjects/c%23/#l12"
    ; "/shares/pröjects/c%23/#l12"
    ; "\\shares/pröjects/c%23/#l12"
    ];
  [%expect
    {|
    Unix:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
    file://shares/pröjects/c%23/#l12 -> //shares/pröjects/c#/
    file:///_:/path -> /_:/path
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> c:/Source/Zürich or Zurich (ˈzjʊərɪk,/Code/resources/app/plugins
    shares/pröjects/c%23/#l12 -> /shares/pröjects/c#/
    /shares/pröjects/c%23/#l12 -> /shares/pröjects/c#/
    \shares/pröjects/c%23/#l12 -> /\shares/pröjects/c#/
    Windows:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> \
    file://shares/pröjects/c%23/#l12 -> \\shares\pröjects\c#\
    file:///_:/path -> \_:\path
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> c:\Source\Zürich or Zurich (ˈzjʊərɪk,\Code\resources\app\plugins
    shares/pröjects/c%23/#l12 -> \shares\pröjects\c#\
    /shares/pröjects/c%23/#l12 -> \shares\pröjects\c#\
    \shares/pröjects/c%23/#l12 -> \\shares\pröjects\c#\
    |}]
;;

let%expect_test "JSON URI strings are normalized" =
  let test_of_string_to_string =
    let test s =
      let uri = Uri.t_of_yojson (`String s) in
      Printf.printf "%s -> %s\n" s (Uri.to_string uri)
    in
    fun s -> run_with_modes (fun () -> List.iter test s)
  in
  test_of_string_to_string
    [ "file://shares/pröjects/c%23/#l12"
    ; "file://sh%c3%a4res/path"
    ; "untitled:c:/Users/jrieken/Code/abc.txt"
    ; "untitled:C:/Users/jrieken/Code/abc.txt"
    ; "/Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js"
    ; "file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins"
    ; "file:foo/bar"
    ; ""
    ; "file://LöC%2FAL/host:8080/projects/"
    ; "file:///pro%2Fjects/"
    ; "vscode://mount/test.ml"
    ];
  [%expect
    {|
    Unix:
    file://shares/pröjects/c%23/#l12 -> file://shares/pr%C3%B6jects/c%23/#l12
    file://sh%c3%a4res/path -> file://sh%C3%A4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:c:/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:C:/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c%3A/Source/Z%C3%BCrich%20or%20Zurich%20%28%CB%88zj%CA%8A%C9%99r%C9%AAk%2C/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
     ->
    file://LöC%2FAL/host:8080/projects/ -> file://l%C3%B6c%2Fal/host%3A8080/projects/
    file:///pro%2Fjects/ -> file:///pro/jects/
    vscode://mount/test.ml -> vscode://mount/test.ml
    Windows:
    file://shares/pröjects/c%23/#l12 -> file://shares/pr%C3%B6jects/c%23/#l12
    file://sh%c3%a4res/path -> file://sh%C3%A4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:c:/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:C:/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c%3A/Source/Z%C3%BCrich%20or%20Zurich%20%28%CB%88zj%CA%8A%C9%99r%C9%AAk%2C/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
     ->
    file://LöC%2FAL/host:8080/projects/ -> file://l%C3%B6c%2Fal/host%3A8080/projects/
    file:///pro%2Fjects/ -> file:///pro/jects/
    vscode://mount/test.ml -> vscode://mount/test.ml
    |}]
;;

let%expect_test "of_string -> to_path" =
  let test_of_string_to_path =
    let test s =
      let uri = Uri.t_of_yojson (`String s) in
      Printf.printf "%s -> %s\n" s (Uri.to_path uri)
    in
    fun s -> run_with_modes (fun () -> List.iter test s)
  in
  test_of_string_to_path
    [ "file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp"
    ; "file://shares/pröjects/c%23/#l12"
    ; "file:///_:/path"
    ; ""
    ; "file://LöC%2FAL/host:8080/projects/"
    ];
  [%expect
    {|
    Unix:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
    file://shares/pröjects/c%23/#l12 -> //shares/pröjects/c#/
    file:///_:/path -> /_:/path
     -> /
    file://LöC%2FAL/host:8080/projects/ -> //löc/al/host:8080/projects/
    Windows:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> \
    file://shares/pröjects/c%23/#l12 -> \\shares\pröjects\c#\
    file:///_:/path -> \_:\path
     -> \
    file://LöC%2FAL/host:8080/projects/ -> \\löc\al\host:8080\projects\
    |}]
;;
