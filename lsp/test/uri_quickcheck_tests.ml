open Base
open Base_quickcheck
module Uri = Lsp.Uri

type atom =
  | A
  | Z
  | Digit
  | Hyphen
  | Dot
  | Underscore
  | Tilde
  | Space
  | Hash
  | Percent
  | Slash
  | Question
  | Colon
  | At
  | Equals
  | Two_byte
  | Four_byte
  | Raw_two_byte
  | Raw_four_byte
  | Raw_space
  | Lone_percent
  | Incomplete_escape
  | Invalid_escape
  | Hidden_escape
[@@deriving quickcheck, sexp_of]

type scheme =
  | File
  | Http
  | Https
  | Untitled
  | Vscode
[@@deriving quickcheck, sexp_of]

module Case = struct
  type t =
    { scheme : scheme
    ; authority : atom list
    ; path : atom list
    ; query : atom list option
    ; fragment : atom list option
    }
  [@@deriving quickcheck, sexp_of]
end

let string_of_scheme = function
  | File -> "file"
  | Http -> "http"
  | Https -> "https"
  | Untitled -> "untitled"
  | Vscode -> "vscode"
;;

(* Mix canonical escapes with raw Unicode, whitespace, and malformed escapes so
   parsing and serialization exercise both valid input and their tolerant paths. *)
let encoded_atom = function
  | A -> "a"
  | Z -> "Z"
  | Digit -> "1"
  | Hyphen -> "-"
  | Dot -> "."
  | Underscore -> "_"
  | Tilde -> "~"
  | Space -> "%20"
  | Hash -> "%23"
  | Percent -> "%25"
  | Slash -> "%2F"
  | Question -> "%3F"
  | Colon -> "%3A"
  | At -> "%40"
  | Equals -> "%3D"
  | Two_byte -> "%C3%A9"
  | Four_byte -> "%F0%9F%98%80"
  | Raw_two_byte -> "é"
  | Raw_four_byte -> "😀"
  | Raw_space -> " "
  | Lone_percent -> "%!"
  | Incomplete_escape -> "%A!"
  | Invalid_escape -> "%GG"
  (* The malformed '%' must not combine with decoded neighbours into a new escape. *)
  | Hidden_escape -> "%%32%36"
;;

let encoded_component atoms = List.map atoms ~f:encoded_atom |> String.concat ~sep:""

let query_atom = function
  | Slash -> "/"
  | Question -> "?"
  | Colon -> ":"
  | At -> "@"
  | Equals -> "="
  | atom -> encoded_atom atom
;;

let query_component atoms = List.map atoms ~f:query_atom |> String.concat ~sep:""

let source ({ scheme; authority; path; query; fragment } : Case.t) =
  let suffix marker = function
    | None -> ""
    | Some atoms ->
      let component =
        if Char.equal marker '?' then query_component else encoded_component
      in
      String.of_char marker ^ component atoms
  in
  string_of_scheme scheme
  ^ "://"
  ^ encoded_component authority
  ^ "/"
  ^ encoded_component path
  ^ suffix '?' query
  ^ suffix '#' fragment
;;

let uri_examples : Case.t list =
  [ { scheme = File
    ; authority = []
    ; path = [ A ]
    ; query = None
    ; fragment = Some [ Space ]
    }
  ; { scheme = File
    ; authority = []
    ; path = [ Digit; Colon; A ]
    ; query = None
    ; fragment = None
    }
  ; { scheme = Untitled
    ; authority = []
    ; path = [ Slash; Z ]
    ; query = None
    ; fragment = None
    }
  ; { scheme = File
    ; authority = []
    ; path = [ A ]
    ; query = Some [ Raw_four_byte; Equals; A ]
    ; fragment = None
    }
  ]
;;

let with_windows_setting windows ~f =
  let previous = !Uri.Private.win32 in
  Exn.protect
    ~f:(fun () ->
      Uri.Private.win32 := windows;
      f ())
    ~finally:(fun () -> Uri.Private.win32 := previous)
;;

let fail source label expected actual =
  failwith
    (Printf.sprintf
       "%s\nsource: %S\nexpected: %S\nactual: %S"
       label
       source
       expected
       actual)
;;

module Wire = struct
  type t = string [@@deriving sexp_of]

  let quickcheck_generator =
    Generator.string_of (Generator.char_uniform_inclusive '\000' '\255')
  ;;

  let quickcheck_observer = Observer.string
  let quickcheck_shrinker = Shrinker.string
end

let byte_examples =
  [ ""
  ; "%"
  ; "%2F"
  ; "%25"
  ; "%GG"
  ; "?x=y#fragment"
  ; "é😀"
  ; String.init 256 ~f:Char.of_int_exn
  ; String.make 4096 '%'
  ]
;;

let check_equivalent left right =
  let check label condition =
    if not condition
    then fail (Uri.to_string left) label (Uri.to_string left) (Uri.to_string right)
  in
  check
    "equivalent URI spellings compare unequal"
    (Uri.equal left right && Uri.equal right left);
  check
    "equal URIs compare differently"
    (Uri.compare left right = 0 && Uri.compare right left = 0);
  check "equal URIs have different hashes" (Uri.hash left = Uri.hash right);
  check "equivalent URIs have different representations" (Poly.equal left right);
  check
    "equivalent URIs have different serializations"
    (String.equal (Uri.to_string left) (Uri.to_string right))
;;

let check_uri_round_trip uri =
  let source = Uri.to_string uri in
  let from_string = Uri.of_string source in
  let from_json =
    Uri.yojson_of_t uri
    |> Yojson.Safe.to_string
    |> Yojson.Safe.from_string
    |> Uri.t_of_yojson
  in
  List.iter [ from_string; from_json ] ~f:(fun parsed ->
    check_equivalent uri parsed;
    if not (String.equal source (Uri.to_string parsed))
    then fail source "URI round trip changed syntax" source (Uri.to_string parsed);
    if
      not
        (Option.equal String.equal (Uri.query uri) (Uri.query parsed)
         && Option.equal String.equal (Uri.fragment uri) (Uri.fragment parsed))
    then failwith "URI round trip changed decoded components";
    List.iter [ false; true ] ~f:(fun windows ->
      with_windows_setting windows ~f:(fun () ->
        let expected = Uri.to_path uri in
        let actual = Uri.to_path parsed in
        if not (String.equal expected actual)
        then fail source "URI round trip changed the filesystem path" expected actual)))
;;

(* An independent byte encoder supplies known-equivalent spellings and an oracle
   for decoding. Never use the production encoder/decoder to build expectations. *)
type spelling =
  | Literal
  | Escaped_upper
  | Escaped_lower

let encode_component spelling bytes =
  String.to_list bytes
  |> List.map ~f:(fun c ->
    match spelling with
    | Literal
      when String.contains
             "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
             c -> String.of_char c
    | Literal | Escaped_upper -> Printf.sprintf "%%%02X" (Char.to_int c)
    | Escaped_lower -> Printf.sprintf "%%%02x" (Char.to_int c))
  |> String.concat
;;

module Components = struct
  type t =
    { scheme : scheme option
    ; authority : Wire.t option
    ; path : Wire.t
    ; query : Wire.t option
    ; fragment : Wire.t option
    }
  [@@deriving quickcheck, sexp_of]

  let render spelling { scheme; authority; path; query; fragment } =
    let encode = encode_component spelling in
    let optional prefix = Option.value_map ~default:"" ~f:(fun s -> prefix ^ encode s) in
    let scheme =
      Option.value_map scheme ~default:"" ~f:(fun scheme ->
        let name = string_of_scheme scheme in
        let name =
          match spelling with
          | Escaped_lower -> String.uppercase name
          | _ -> name
        in
        name ^ ":")
    in
    scheme
    ^ optional "//" authority
    ^ "/"
    ^ encode path
    ^ optional "?" query
    ^ optional "#" fragment
  ;;
end

module Input = struct
  type t =
    | Raw of Wire.t
    | Structured of Components.t
    | Path of Wire.t
  [@@deriving quickcheck, sexp_of]

  let uri = function
    | Raw source -> Uri.of_string source
    | Structured components -> Components.render Literal components |> Uri.of_string
    | Path bytes ->
      with_windows_setting false ~f:(fun () -> Uri.of_path ("/root/" ^ bytes))
  ;;
end

let%expect_test "URI values survive string and JSON serialization" =
  Test.run_exn
    (module Input)
    ~examples:(List.map byte_examples ~f:(fun s -> Input.Raw s))
    ~f:(fun input -> check_uri_round_trip (Input.uri input));
  [%expect {| |}]
;;

module Equivalent_case = struct
  type t = Components.t * Input.t [@@deriving quickcheck, sexp_of]
end

let%expect_test "equivalent spellings preserve identity, hash, and ordering" =
  Test.run_exn
    (module Equivalent_case)
    ~f:(fun (components, other) ->
      let uris =
        List.map [ Literal; Escaped_upper; Escaped_lower ] ~f:(fun spelling ->
          Components.render spelling components |> Uri.of_string)
      in
      let other = Input.uri other in
      List.iter uris ~f:(fun left ->
        List.iter uris ~f:(fun right ->
          check_equivalent left right;
          if
            Int.compare (Uri.compare left other) 0
            <> Int.compare (Uri.compare right other) 0
            || Int.compare (Uri.compare other left) 0
               <> Int.compare (Uri.compare other right) 0
          then failwith "equivalent URIs are not interchangeable in ordering")));
  [%expect {| |}]
;;

module Triple = struct
  type t = Input.t * Input.t * Input.t [@@deriving quickcheck, sexp_of]
end

let%expect_test "URI comparison is a total order consistent with equality and hash" =
  Test.run_exn
    (module Triple)
    ~f:(fun (a, b, c) ->
      let a, b, c = Input.uri a, Input.uri b, Input.uri c in
      List.iter [ a; b; c ] ~f:(fun left ->
        if not (Uri.equal left left && Uri.compare left left = 0)
        then failwith "URI identity is not reflexive";
        List.iter [ a; b; c ] ~f:(fun right ->
          let comparison = Int.compare (Uri.compare left right) 0 in
          if comparison <> -Int.compare (Uri.compare right left) 0
          then failwith "URI comparison is not antisymmetric";
          if not (Bool.equal (Uri.equal left right) (comparison = 0))
          then failwith "URI equality disagrees with comparison";
          if Uri.equal left right then check_equivalent left right));
      (* Check every orientation, not only triples that happen to arrive sorted. *)
      List.iter
        [ a, b, c; b, c, a; c, a, b ]
        ~f:(fun (a, b, c) ->
          let ab, bc, ac = Uri.compare a b, Uri.compare b c, Uri.compare a c in
          if (ab <= 0 && bc <= 0 && ac > 0) || (ab >= 0 && bc >= 0 && ac < 0)
          then failwith "URI comparison is not transitive"));
  [%expect {| |}]
;;

let%expect_test "percent-decoding recovers arbitrary component bytes exactly once" =
  Test.run_exn
    (module Wire)
    ~examples:byte_examples
    ~f:(fun bytes ->
      List.iter [ Literal; Escaped_upper; Escaped_lower ] ~f:(fun spelling ->
        let encoded = encode_component spelling bytes in
        let source = "x:/path?" ^ encoded ^ "#" ^ encoded in
        let uri = Uri.of_string source in
        List.iter
          [ Uri.query uri; Uri.fragment uri ]
          ~f:(fun actual ->
            if not (Option.equal String.equal (Some bytes) actual)
            then
              fail
                source
                "component decoding changed bytes"
                bytes
                (Option.value actual ~default:"<absent>"));
        let canonical = encode_component Literal bytes in
        let expected = "x:/path?" ^ canonical ^ "#" ^ canonical in
        if not (String.equal expected (Uri.to_string uri))
        then
          fail
            source
            "component normalization changed syntax"
            expected
            (Uri.to_string uri)));
  [%expect {| |}]
;;

let%expect_test "reserved escapes and empty delimiters remain significant" =
  Test.run_exn
    (module Wire)
    ~examples:byte_examples
    ~f:(fun bytes ->
      let middle = String.length bytes / 2 in
      let prefix = encode_component Escaped_upper (String.prefix bytes middle) in
      let suffix = encode_component Escaped_upper (String.drop_prefix bytes middle) in
      let distinct left right =
        let a, b = Uri.of_string left, Uri.of_string right in
        if Uri.equal a b || Uri.equal b a || Uri.compare a b = 0 || Uri.compare b a = 0
        then fail left "meaningful URI syntax was conflated" left right
      in
      (* File paths deliberately have different equivalence rules; test reserved
       path characters on a non-file URI, and suffixes on both schemes. *)
      List.iter
        [ "https://host/"
        ; "https://host/?"
        ; "https://host/#"
        ; "file:///root/file.ml?"
        ; "file:///root/file.ml#"
        ]
        ~f:(fun base ->
          String.iter "/?:@!$&'()*+,;=" ~f:(fun c ->
            let literal = base ^ prefix ^ String.of_char c ^ suffix in
            let escaped =
              base ^ prefix ^ encode_component Escaped_upper (String.of_char c) ^ suffix
            in
            distinct literal escaped));
      List.iter [ "x:/"; "file:///root/" ] ~f:(fun base ->
        let source = base ^ prefix ^ suffix in
        distinct source (source ^ "?");
        distinct source (source ^ "#");
        distinct (source ^ "?") (source ^ "?#"));
      distinct ("x:/" ^ prefix ^ suffix) ("x:///" ^ prefix ^ suffix);
      distinct ("x:/" ^ prefix ^ suffix) ("y:/" ^ prefix ^ suffix);
      (* Meaningfully different data must not compare equal either, even if a
       comparator accidentally drops one component from its key. *)
      List.iter
        [ "https://"; "https://host/"; "https://host/?"; "https://host/#" ]
        ~f:(fun base ->
          distinct (base ^ "a" ^ prefix ^ suffix) (base ^ "b" ^ prefix ^ suffix)));
  [%expect {| |}]
;;

let check_normalization source =
  let uri = Uri.of_string source in
  check_equivalent uri (Uri.t_of_yojson (`String source));
  check_uri_round_trip uri
;;

let%expect_test "URI normalization is idempotent regardless of input syntax" =
  Test.run_exn
    (module Wire)
    ~examples:
      ([ ""
       ; "?#"
       ; "x:/"
       ; "x:///"
       ; "//host"
       ; "file:relative"
       ; "x:?%%36%31"
       ; "x:%%34%31"
       ; "#%%32%36"
       ; "x:?%2%36"
       ]
       @ byte_examples)
    ~f:check_normalization;
  [%expect {| |}]
;;

let%expect_test "structured URI strings and JSON normalize identically and idempotently" =
  Test.run_exn
    (module Case)
    ~examples:uri_examples
    ~f:(fun case -> check_normalization (source case));
  [%expect {| |}]
;;

let%expect_test "query and fragment do not change the filesystem path" =
  Test.run_exn
    (module Case)
    ~examples:uri_examples
    ~f:(fun case ->
      let source_text = source case in
      let without_suffix = source { case with query = None; fragment = None } in
      List.iter [ false; true ] ~f:(fun windows ->
        with_windows_setting windows ~f:(fun () ->
          let expected = Uri.of_string without_suffix |> Uri.to_path in
          let actual = Uri.of_string source_text |> Uri.to_path in
          if not (String.equal expected actual)
          then fail source_text "URI suffix changed the filesystem path" expected actual)));
  [%expect {| |}]
;;

module Path = struct
  type t = atom list [@@deriving quickcheck, sexp_of]
end

let decoded_atom = function
  | A -> "a"
  | Z -> "Z"
  | Digit -> "1"
  | Hyphen -> "-"
  | Dot -> "."
  | Underscore -> "_"
  | Tilde -> "~"
  | Space -> " "
  | Hash -> "#"
  | Percent -> "%"
  | Slash -> "/"
  | Question -> "?"
  | Colon -> ":"
  | At -> "@"
  | Equals -> "="
  | Two_byte | Raw_two_byte -> "é"
  | Four_byte | Raw_four_byte -> "😀"
  | Raw_space -> " "
  | Lone_percent -> "%!"
  | Incomplete_escape -> "%A!"
  | Invalid_escape -> "%GG"
  | Hidden_escape -> "%26"
;;

let decoded_component atoms = List.map atoms ~f:decoded_atom |> String.concat ~sep:""

let%expect_test "query and fragment components are percent-decoded" =
  Test.run_exn
    (module Case)
    ~examples:uri_examples
    ~f:(fun (case : Case.t) ->
      let source = source case in
      let uri = Uri.of_string source in
      let check name expected actual =
        if not (Option.equal String.equal expected actual)
        then
          fail
            source
            (name ^ " component was not decoded")
            (Option.value expected ~default:"<none>")
            (Option.value actual ~default:"<none>")
      in
      check "query" (Option.map case.query ~f:decoded_component) (Uri.query uri);
      check "fragment" (Option.map case.fragment ~f:decoded_component) (Uri.fragment uri));
  [%expect {| |}]
;;

let round_trip_path ~windows path =
  with_windows_setting windows ~f:(fun () ->
    let uri = Uri.of_path path in
    let direct = Uri.to_path uri in
    let serialized = Uri.to_string uri in
    check_uri_round_trip uri;
    let round_trip = Uri.of_string serialized |> Uri.to_path in
    direct, round_trip)
;;

let check_path_round_trip ~windows path expected =
  let direct, round_trip = round_trip_path ~windows path in
  if not (String.equal expected direct)
  then fail path "filesystem path did not round trip" expected direct;
  if not (String.equal direct round_trip)
  then fail path "URI serialization changed the filesystem path" direct round_trip
;;

let%expect_test "absolute Unix filesystem paths round trip" =
  Test.run_exn
    (module Path)
    ~f:(fun atoms ->
      let path = "/root/" ^ decoded_component atoms in
      check_path_round_trip ~windows:false path path);
  [%expect {| |}]
;;

let%expect_test "Windows drive filesystem paths round trip" =
  Test.run_exn
    (module Path)
    ~f:(fun atoms ->
      let suffix = decoded_component atoms |> String.tr ~target:'/' ~replacement:'\\' in
      let path = "C:\\root\\" ^ suffix in
      let expected = "c:\\root\\" ^ suffix in
      check_path_round_trip ~windows:true path expected);
  [%expect {| |}]
;;

let%expect_test "Windows UNC filesystem paths round trip" =
  Test.run_exn
    (module Path)
    ~f:(fun atoms ->
      let suffix = decoded_component atoms |> String.tr ~target:'/' ~replacement:'\\' in
      List.iter [ "server"; "SeRvEr" ] ~f:(fun server ->
        let path = "\\\\" ^ server ^ "\\share\\" ^ suffix in
        let expected = "\\\\server\\share\\" ^ suffix in
        check_path_round_trip ~windows:true path expected));
  [%expect {| |}]
;;

let%expect_test "arbitrary byte paths round trip and match equivalent file URIs" =
  (* These are pure codec tests; even NUL and invalid UTF-8 are intentional, and
     no files with these names are created. Only platform separators, drive letters,
     and UNC authority case are normalized. *)
  Test.run_exn
    (module Wire)
    ~examples:byte_examples
    ~f:(fun bytes ->
      let unix = "/root/" ^ bytes in
      let windows = String.tr bytes ~target:'/' ~replacement:'\\' in
      let slash_suffix = String.tr windows ~target:'\\' ~replacement:'/' in
      let quote = encode_component Escaped_lower in
      let encode_path path =
        String.split path ~on:'/'
        |> List.map ~f:(encode_component Literal)
        |> String.concat ~sep:"/"
      in
      List.iter
        [ ( false
          , unix
          , unix
          , "file:///root/" ^ encode_path bytes
          , "FILE:/" ^ quote ("root/" ^ bytes) )
        ; ( true
          , "C:\\root\\" ^ windows
          , "c:\\root\\" ^ windows
          , "file:///c%3A/root/" ^ encode_path slash_suffix
          , "FILE:/C%3a/" ^ quote ("root/" ^ slash_suffix) )
        ; ( true
          , "\\\\SeRvEr\\share\\" ^ windows
          , "\\\\server\\share\\" ^ windows
          , "file://server/share/" ^ encode_path slash_suffix
          , "FILE://sErVeR/" ^ quote ("share/" ^ slash_suffix) )
        ]
        ~f:(fun (windows, path, expected, wire, alias) ->
          check_path_round_trip ~windows path expected;
          with_windows_setting windows ~f:(fun () ->
            let uri = Uri.of_path path in
            if not (String.equal wire (Uri.to_string uri))
            then
              fail
                path
                "filesystem path was not correctly escaped"
                wire
                (Uri.to_string uri);
            if Option.is_some (Uri.query uri) || Option.is_some (Uri.fragment uri)
            then failwith "filename bytes introduced URI suffixes";
            check_equivalent uri (Uri.of_string alias);
            check_equivalent uri (Uri.t_of_yojson (`String alias)))));
  [%expect {| |}]
;;
