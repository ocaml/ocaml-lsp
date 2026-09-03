open Lsp
open Types

let print_json json = Yojson.Safe.pretty_to_string json |> Stdlib.print_endline

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
  print_json
    (NotebookDocumentSyncOptions.yojson_of_t
       (NotebookDocumentSyncOptions.create ~notebookSelector:[] ()));
  print_decoded
    "sync options"
    NotebookDocumentSyncOptions.t_of_yojson
    NotebookDocumentSyncOptions.yojson_of_t
    (`Assoc [ "notebookSelector", `List [] ]);
  print_decoded
    "registration options"
    NotebookDocumentSyncRegistrationOptions.t_of_yojson
    NotebookDocumentSyncRegistrationOptions.yojson_of_t
    (`Assoc [ "id", `String "notebook-registration"; "notebookSelector", `List [] ]);
  print_decoded
    "notebook filter"
    NotebookDocumentFilter.t_of_yojson
    NotebookDocumentFilter.yojson_of_t
    (`Assoc [ "notebookType", `String "jupyter-notebook" ]);
  [%expect
    {|
    { "notebookSelector": [] }
    sync options:
    { "notebookSelector": [] }
    registration options:
    { "id": "notebook-registration", "notebookSelector": [] }
    notebook filter:
    { "notebookType": "jupyter-notebook" }
    |}]
;;
