let () = Printexc.record_backtrace false

let%expect_test "toplevel exception" =
  (match
     Lev_fiber.run @@ fun () ->
     print_endline "raising Exit";
     let _ = raise Exit in
     Fiber.return ()
   with
  | Error Deadlock | Ok () -> assert false
  | Error Already_reported -> print_endline "raised Exit"
  | Error (Aborted _) -> assert false);
  [%expect
    {|
    raising Exit
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | Stdlib.Exit
    \-----------------------------------------------------------------------

    raised Exit |}]

let%expect_test "" =
  (match
     Lev_fiber.run @@ fun () ->
     Fiber.fork_and_join_unit
       (fun () ->
         print_endline "t1: raising";
         raise Exit)
       (fun () ->
         print_endline "t2: running";
         Fiber.return ())
   with
  | Error Deadlock | Ok () -> assert false
  | Error Already_reported -> print_endline "raised Exit"
  | Error (Aborted _) -> assert false);
  [%expect
    {|
    t1: raising
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | Stdlib.Exit
    \-----------------------------------------------------------------------

    t2: running
    raised Exit |}]
