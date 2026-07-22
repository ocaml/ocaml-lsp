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

(* Render every non-unreserved atom as an escape so generated component text is
   syntactically valid and parsing, rather than the generator, determines its
   decoded representation. *)
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
;;

let encoded_component atoms = List.map atoms ~f:encoded_atom |> String.concat ~sep:""

let source ({ scheme; authority; path; query; fragment } : Case.t) =
  let suffix marker = function
    | None -> ""
    | Some atoms -> String.of_char marker ^ encoded_component atoms
  in
  string_of_scheme scheme
  ^ "://"
  ^ encoded_component authority
  ^ "/"
  ^ encoded_component path
  ^ suffix '?' query
  ^ suffix '#' fragment
;;

let with_windows_setting windows ~f =
  let previous = !(Uri.Private.win32) in
  Exn.protect ~f:(fun () -> Uri.Private.win32 := windows; f ()) ~finally:(fun () ->
    Uri.Private.win32 := previous)
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

let%expect_test "query and fragment do not change the filesystem path" =
  Test.run_exn
    (module Case)
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
  | Two_byte -> "é"
  | Four_byte -> "😀"
;;

let decoded_component atoms = List.map atoms ~f:decoded_atom |> String.concat ~sep:""

let%expect_test "query components are percent-decoded" =
  Test.run_exn
    (module Case)
    ~f:(fun (case : Case.t) ->
      let source = source case in
      let expected = Option.map case.query ~f:decoded_component in
      let actual = Uri.of_string source |> Uri.query in
      if not (Option.equal String.equal expected actual)
      then
        fail
          source
          "query component was not decoded"
          (Option.value expected ~default:"<none>")
          (Option.value actual ~default:"<none>"));
  [%expect {| |}]
;;

let%expect_test "absolute Unix filesystem paths round trip" =
  Test.run_exn
    (module Path)
    ~f:(fun atoms ->
      let path = "/root/" ^ decoded_component atoms in
      let actual =
        with_windows_setting false ~f:(fun () -> Uri.of_path path |> Uri.to_path)
      in
      if not (String.equal path actual)
      then fail path "filesystem path did not round trip" path actual);
  [%expect {| |}]
;;

let%expect_test "Windows drive filesystem paths round trip" =
  Test.run_exn
    (module Path)
    ~f:(fun atoms ->
      let suffix = decoded_component atoms |> String.tr ~target:'/' ~replacement:'\\' in
      let path = "C:\\root\\" ^ suffix in
      let expected = "c:\\root\\" ^ suffix in
      let actual = with_windows_setting true ~f:(fun () -> Uri.of_path path |> Uri.to_path) in
      if not (String.equal expected actual)
      then fail path "filesystem path did not round trip" expected actual);
  [%expect {| |}]
;;
