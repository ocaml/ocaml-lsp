open! Test.Import

let%expect_test "with-ppx" =
  let project =
    Preprocessor_helpers.setup
      ~name:"for_ppx"
      ~fixture:"ocaml-lsp-server/test/e2e-new/for_ppx.ml"
      ~dune_file:
        {|(library
 (name for_ppx)
 (modules for_ppx)
 (inline_tests)
 (preprocess
  (pps ppx_expect)))
|}
  in
  (* We will call 'hover' on the last line of this very file *)
  let position = Position.create ~line:2 ~character:5 in
  (* We need to wait for the first diagnostics *)
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    let on_notification (_ : _ Client.t) (n : Client.in_notification) =
      match n with
      | PublishDiagnostics diag ->
        printfn "Received %i diagnostics" (List.length diag.diagnostics);
        List.iter diag.diagnostics ~f:(fun (d : Diagnostic.t) ->
          match d.message with
          | `String m -> print_endline m
          | `MarkupContent _ -> assert false);
        Fiber.Ivar.fill diagnostics ()
      | _ -> Fiber.return ()
    in
    Client.Handler.make ~on_notification ()
  in
  let output =
    Preprocessor_helpers.hover
      ~prep:(fun _ -> Fiber.Ivar.read diagnostics)
      ~handler
      ~project
      ~position
      ~capture:(fun () -> [%expect.output])
      ()
  in
  let (_ : string) = [%expect.output] in
  print_endline output;
  [%expect
    {xxx|
    Received 0 diagnostics
    {
      "contents": {
        "value": "(* ppx expect expansion *)\nPpx_expect_runtime.Current_file.unset ()",
        "language": "ocaml"
      },
      "range": {
        "end": { "character": 16, "line": 2 },
        "start": { "character": 2, "line": 2 }
      }
    }
    |xxx}]
;;
