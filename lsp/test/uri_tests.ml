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
    file:///foo?x=y -> /foo?x=y
    query: x=y
    http://xyz?foo# -> /?foo
    query: foo
    http://xxx? -> /?
    query:
    http://xyz?ab%3D1%23 -> /?ab=1#
    query: ab=1#
    Windows:
    file:///Users/foo -> \Users\foo
    file:///c:/Users/foo -> c:\Users\foo
    file:///foo?x=y -> \foo?x=y
    query: x=y
    http://xyz?foo# -> \?foo
    query: foo
    http://xxx? -> \?
    query:
    http://xyz?ab%3D1%23 -> \?ab=1#
    query: ab=1# |}]
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

let%expect_test "of_string -> to_string" =
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
    file://shares/pröjects/c%23/#l12 -> file://shares/pr%C3%B6jects/c%23/
    file://sh%c3%a4res/path -> file://sh%C3%A4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c%3A/Source/Z%C3%BCrich%20or%20Zurich%20%28%CB%88zj%CA%8A%C9%99r%C9%AAk%2C/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
     -> file:///
    file://LöC%2FAL/host:8080/projects/ -> file://l%C3%B6c%2Fal/host%3A8080/projects/
    file:///pro%2Fjects/ -> file:///pro/jects/
    vscode://mount/test.ml -> vscode://mount/test.ml
    Windows:
    file://shares/pröjects/c%23/#l12 -> file://shares/pr%C3%B6jects/c%23/
    file://sh%c3%a4res/path -> file://sh%C3%A4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c%3A/Source/Z%C3%BCrich%20or%20Zurich%20%28%CB%88zj%CA%8A%C9%99r%C9%AAk%2C/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
     -> file:///
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
      file://LöC%2FAL/host:8080/projects/ -> //LöC/AL/host:8080/projects/
      Windows:
      file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> \
      file://shares/pröjects/c%23/#l12 -> \\shares\pröjects\c#\
      file:///_:/path -> \_:\path
       -> \
      file://LöC%2FAL/host:8080/projects/ -> \\LöC\AL\host:8080\projects\
      |}]
;;
