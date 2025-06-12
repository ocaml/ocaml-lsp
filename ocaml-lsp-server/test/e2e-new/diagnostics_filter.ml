open Async
open Test.Import

let print_diagnostics
  ?(prep = fun _ -> Fiber.return ())
  ?(print_range : bool = false)
  ?(path : string = "foo.ml")
  (source : string)
  =
  Lsp_helpers.open_document_with_diagnostics_callback
    ~prep
    ~path
    ~source
    ~diagnostics_callback:(fun diagnostics ->
      print_endline
        (String.concat
           ~sep:", "
           (List.map diagnostics.diagnostics ~f:(fun (d : Diagnostic.t) ->
              let range_message =
                if print_range
                then "\n" ^ Ocaml_lsp_server.Testing.Range.to_string d.range ^ "\n"
                else ""
              in
              match d.message with
              | `String m -> m ^ range_message
              | `MarkupContent { value; _ } -> value ^ range_message))))
    ()
;;

let change_config client params = Client.notification client (ChangeConfiguration params)

let%expect_test "receiving diagnostics" =
  let source =
    {ocaml|
let x = Foo.oh_no
let y = garbage
;;
|ocaml}
  in
  let%map () = print_diagnostics ~path:"../this-directory-does-not-exist/foo.ml" source in
  [%expect
    {| No config found for file ocaml-lsp-server/test/e2e-new/this-directory-does-not-exist/foo.ml. Try calling 'dune build'. |}]
;;

let%expect_test "doesn't add other diagnostics if syntax errors" =
  let source =
    {ocaml|
  let x = "" in

  let () = 1
  |ocaml}
  in
  let%map () = print_diagnostics source in
  [%expect {| Syntax error, expecting `in' |}]
;;

let%expect_test "shorten diagnostics" =
  let source =
    {ocaml|
    let x: unit = fun () ->




      ()

    let () = match true with

    | false -> ()
  |ocaml}
  in
  let req enable =
    print_diagnostics
      ~prep:(fun client ->
        change_config
          client
          (DidChangeConfigurationParams.create
             ~settings:
               (`Assoc [ "shortenMerlinDiagnostics", `Assoc [ "enable", `Bool enable ] ])))
      ~print_range:true
      source
  in
  let%bind.Deferred () = req true in
  let%bind.Deferred () = req false in
  [%expect
    {|
    Warning 8: this pattern-matching is not exhaustive.
    Here is an example of a case that is not matched:
    true
    ((8, 13), (9, 0))
    , This expression should not be a function, the expected type is
    unit
    ((1, 18), (2, 0))

    Warning 8: this pattern-matching is not exhaustive.
    Here is an example of a case that is not matched:
    true
    ((8, 13), (10, 17))
    , This expression should not be a function, the expected type is
    unit
    ((1, 18), (6, 8))
    |}];
  return ()
;;

(* TODO: I would like to test
   [Diagnostics.refresh_merlin_diagnostics_if_doc_has_cached_errors], but there's no way
   to replicate the behavior of "having merlin report diagnostics in a file and then change
   what diagnostics Merlin should display for that file" without either modifying another
   file and having a Dune update cause a refresh and then update the diagnostics in that
   file, or having access to the LSP as more than just a process. This means testing
   that is blocked until Dune testing support is released. *)
