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

let canonical source = Uri.of_string source |> Uri.to_string

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

let%expect_test "URI serialization reaches a fixed point" =
  Test.run_exn
    (module Case)
    ~examples:uri_examples
    ~f:(fun case ->
      let source = source case in
      let once = canonical source in
      let twice = canonical once in
      if not (String.equal once twice)
      then fail source "URI serialization is not idempotent" once twice);
  [%expect {| |}]
;;

let%expect_test "URI JSON serialization preserves canonical values" =
  Test.run_exn
    (module Case)
    ~examples:uri_examples
    ~f:(fun case ->
      let source = source case |> canonical in
      let uri = Uri.of_string source in
      let round_trip = Uri.t_of_yojson (Uri.yojson_of_t uri) in
      if not (Uri.equal uri round_trip)
      then fail source "JSON round trip changed the URI" source (Uri.to_string round_trip));
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
      let path = "\\\\server\\share\\" ^ suffix in
      check_path_round_trip ~windows:true path path);
  [%expect {| |}]
;;
