open Lsp
open Types

let print_json json = Yojson.Safe.pretty_to_string json |> Stdlib.print_endline

let check_decode label decode json =
  Stdlib.Printf.printf "%s: " label;
  match decode json with
  | _ -> Stdlib.print_endline "accepted"
  | exception _ -> Stdlib.print_endline "rejected"
;;

let print_decoded label decode encode json =
  Stdlib.Printf.printf "%s:\n" label;
  decode json |> encode |> print_json
;;

let%expect_test "document filter wire contract" =
  let encoded =
    DocumentFilter.yojson_of_t
      (`TextDocumentFilter
          (`TextDocumentFilterLanguage
              (TextDocumentFilterLanguage.create ~language:"ocaml" ())))
  in
  print_json encoded;
  print_decoded
    "language filter"
    DocumentFilter.t_of_yojson
    DocumentFilter.yojson_of_t
    (`Assoc [ "language", `String "ocaml" ]);
  print_decoded
    "relative pattern"
    RelativePattern.t_of_yojson
    RelativePattern.yojson_of_t
    (`Assoc [ "baseUri", `String "file:///workspace"; "pattern", `String "**/*.ml" ]);
  [%expect
    {|
    { "language": "ocaml" }
    language filter:
    { "language": "ocaml" }
    relative pattern:
    { "baseUri": "file:///workspace", "pattern": "**/*.ml" }
    |}]
;;

let%expect_test "notebook sync wire contract" =
  print_json (NotebookDocumentSyncOptions.yojson_of_t ());
  check_decode
    "sync options"
    NotebookDocumentSyncOptions.t_of_yojson
    (`Assoc [ "notebookSelector", `List [] ]);
  check_decode
    "registration options"
    NotebookDocumentSyncRegistrationOptions.t_of_yojson
    (`Assoc [ "id", `String "notebook-registration"; "notebookSelector", `List [] ]);
  check_decode
    "notebook filter"
    NotebookDocumentFilter.t_of_yojson
    (`Assoc [ "notebookType", `String "jupyter-notebook" ]);
  [%expect
    {|
    null
    sync options: rejected
    registration options: rejected
    notebook filter: rejected
    |}]
;;
