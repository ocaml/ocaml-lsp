let current_mask () = Unix.sigprocmask Unix.SIG_BLOCK [] |> List.sort_uniq Int.compare

let () =
  let original =
    Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigusr1; Sys.sigchld; Sys.sigusr2 ]
  in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.sigprocmask Unix.SIG_SETMASK original))
    (fun () ->
       let before = current_mask () in
       let result = Lev_fiber.run (fun () -> Fiber.return 42) in
       assert (result = Ok 42);
       assert (current_mask () = before))
;;
