open! Test.Import

let%expect_test "with-pp" =
  let project =
    Preprocessor_helpers.setup
      ~name:"for_pp"
      ~fixture:"ocaml-lsp-server/test/e2e-new/for_pp.ml"
      ~dune_file:
        {|(library
 (name for_pp)
 (modules for_pp)
 (preprocess
  (action
   (run sed "s/world/universe/g" %{input-file}))))
|}
  in
  let position = Position.create ~line:0 ~character:9 in
  let handler =
    Client.Handler.make
      ~on_notification:(fun client _notification ->
        Client.state client;
        Fiber.return ())
      ()
  in
  let output =
    Preprocessor_helpers.hover
      ~handler
      ~project
      ~position
      ~capture:(fun () -> [%expect.output])
      ()
  in
  let (_ : string) = [%expect.output] in
  print_endline output;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "type universe" },
      "range": {
        "end": { "character": 13, "line": 0 },
        "start": { "character": 0, "line": 0 }
      }
    }|}]
;;
