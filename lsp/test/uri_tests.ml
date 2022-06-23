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
    ; "a.file"
    ; "/Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js"
    ];
  [%expect
    {|
    Unix:
    c:/win/path -> file:///c:/win/path
    C:/win/path -> file:///C:/win/path
    c:/win/path/ -> file:///c:/win/path/
    /c:/win/path -> file:///c:/win/path
    c:\win\path -> file:///c:/win/path
    c:\win/path -> file:///c:/win/path
    \\localhost\c$\GitDevelopment\express -> file:////localhost/c$/GitDevelopment/express
    c:\test with %\path -> file:///c:/test with %/path
    c:\test with %25\path -> file:///c:/test with %25/path
    c:\test with %25\c#code -> file:///c:/test with %25/c#code
    \\shäres\path\c#\plugin.json -> file:////shäres/path/c#/plugin.json
    a.file -> file:///a.file
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js
    Windows:
    c:/win/path -> file:///c:/win/path
    C:/win/path -> file:///C:/win/path
    c:/win/path/ -> file:///c:/win/path/
    /c:/win/path -> file:///c:/win/path
    c:\win\path -> file:///c:/win/path
    c:\win/path -> file:///c:/win/path
    \\localhost\c$\GitDevelopment\express -> file:////localhost/c$/GitDevelopment/express
    c:\test with %\path -> file:///c:/test with %/path
    c:\test with %25\path -> file:///c:/test with %25/path
    c:\test with %25\c#code -> file:///c:/test with %25/c#code
    \\shäres\path\c#\plugin.json -> file:////shäres/path/c#/plugin.json
    a.file -> file:///a.file
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js
    |}]

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
    ; "\\\\shäres\\path\\c#\\plugin.json"
    ; "c:\\test with %\\path"
    ; "c:\\test with %25\\c#code"
    ];
  [%expect
    {|
    Unix:
    c:/win/path -> /c:/win/path
    c:/win/path/ -> /c:/win/path/
    C:/win/path -> /C:/win/path
    /c:/win/path -> /c:/win/path
    ./c/win/path -> /./c/win/path
    c:\win\path -> /c:/win/path
    c:\win/path -> /c:/win/path
    \\localhost\c$\GitDevelopment\express -> ///localhost/c$/GitDevelopment/express
    \\shares -> ///shares
    \\shäres\path\c#\plugin.json -> ///shäres/path/c#/plugin.json
    c:\test with %\path -> /c:/test with %/path
    c:\test with %25\c#code -> /c:/test with %25/c#code
    Windows:
    c:/win/path -> c:/win/path
    c:/win/path/ -> c:/win/path/
    C:/win/path -> C:/win/path
    /c:/win/path -> c:/win/path
    ./c/win/path -> ./c/win/path
    c:\win\path -> c:/win/path
    c:\win/path -> c:/win/path
    \\localhost\c$\GitDevelopment\express -> //localhost/c$/GitDevelopment/express
    \\shares -> //shares
    \\shäres\path\c#\plugin.json -> //shäres/path/c#/plugin.json
    c:\test with %\path -> c:/test with %/path
    c:\test with %25\c#code -> c:/test with %25/c#code
    |}]

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
    ];
  [%expect
    {|
    Unix:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
    file://shares/pröjects/c%23/#l12 -> /pröjects/c%23/#l12
    file:///_:/path -> /_:/path
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> /c:/Source/Z%C3%BCrich or Zurich (%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins
    Windows:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp ->
    file://shares/pröjects/c%23/#l12 -> pröjects/c%23/#l12
    file:///_:/path -> _:/path
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> c:/Source/Z%C3%BCrich or Zurich (%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins
    |}]

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
    ];
  [%expect
    {|
    Unix:
    file://shares/pröjects/c%23/#l12 -> file://shares/pröjects/c%23/#l12
    file://sh%c3%a4res/path -> file://sh%c3%a4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:///c:/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:///C:/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> ///Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c:/Source/Z%C3%BCrich or Zurich (%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
    Windows:
    file://shares/pröjects/c%23/#l12 -> file://shares/pröjects/c%23/#l12
    file://sh%c3%a4res/path -> file://sh%c3%a4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:///c:/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:///C:/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> ///Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c:/Source/Z%C3%BCrich or Zurich (%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
    |}]

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
    ];
  [%expect
    {|
      Unix:
      file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
      file://shares/pröjects/c%23/#l12 -> /pröjects/c%23/#l12
      file:///_:/path -> /_:/path
      Windows:
      file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp ->
      file://shares/pröjects/c%23/#l12 -> pröjects/c%23/#l12
      file:///_:/path -> _:/path
      |}]
