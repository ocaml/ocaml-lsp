module Dune = Ocaml_lsp_server.For_tests.Dune

let run fiber = Lev_fiber.run (fun () -> fiber) |> Lev_fiber.Error.ok_exn

let%expect_test "cleanup runs after reporting an RPC error" =
  let cleanups = ref 0 in
  run
    (Dune.run_with_cleanup
       ~run:(fun () -> Fiber.of_thunk (fun () -> raise Exit))
       ~cleanup:(fun () ->
         incr cleanups;
         Fiber.return ())
       ~on_error:(fun _ ->
         Printf.printf "cleanups while reporting: %d\n" !cleanups;
         Fiber.return ()));
  Printf.printf "total cleanups: %d\n" !cleanups;
  [%expect
    {|
    cleanups while reporting: 0
    total cleanups: 1
    |}]
;;
