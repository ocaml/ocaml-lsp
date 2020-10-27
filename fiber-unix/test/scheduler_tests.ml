open! Import
module S = Fiber_unix.Scheduler

let%expect_test "scheduler starts and runs a fiber" =
  let s = S.create () in
  S.run s
    (Fiber.of_thunk (fun () ->
         print_endline "running";
         Fiber.return ()));
  [%expect {|
    running |}]

let%expect_test "run an async task and wait it for it to finish" =
  let s = S.create () in
  let th = S.create_thread s in
  let async () =
    S.async th (fun () -> print_endline "running in a different thread")
  in
  let run () =
    print_endline "running in scheduler";
    let task = async () in
    let+ res = S.await_no_cancel task in
    match res with
    | Error _ -> assert false
    | Ok () ->
      print_endline "finished running task";
      S.stop th;
      print_endline "stopped thread"
  in
  S.run s (Fiber.of_thunk run);
  [%expect
    {|
    running in scheduler
    running in a different thread
    finished running task
    stopped thread |}]

let%expect_test "create timer & schedule task" =
  let s = S.create () in
  let timer = S.create_timer s ~delay:0.001 in
  let run () =
    print_endline "scheduling timer";
    let+ res =
      S.schedule timer (fun () ->
          print_endline "timer running";
          Fiber.return ())
    in
    match res with
    | Error `Cancelled -> assert false
    | Ok () -> print_endline "timer done"
  in
  S.run s (Fiber.of_thunk run);
  [%expect {|
    scheduling timer
    timer running
    timer done |}]

let%expect_test "create timer & schedule task & cancel it" =
  let s = S.create () in
  let timer = S.create_timer s ~delay:3.0 in
  let run () =
    print_endline "scheduling timer";
    let task =
      S.schedule timer (fun () ->
          print_endline "timer running";
          Fiber.return ())
    in
    Fiber.fork_and_join_unit
      (fun () ->
        let+ res = task in
        match res with
        | Error `Cancelled -> print_endline "timer cancelled successfully"
        | Ok () -> assert false)
      (fun () -> S.cancel_timer timer)
  in
  S.run s (Fiber.of_thunk run);
  [%expect {|
    scheduling timer
    timer cancelled successfully |}]

let%expect_test "create multiple timers" =
  let s = S.create () in
  let timer () = S.create_timer s ~delay:0.001 in
  let run () =
    Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun i ->
        let timer = timer () in
        printf "%d: scheduling timer\n" i;
        let+ res =
          S.schedule timer (fun () ->
              printf "%d: timer running\n" i;
              Fiber.return ())
        in
        match res with
        | Error `Cancelled -> assert false
        | Ok () -> printf "%d: timer done\n" i)
  in
  S.run s (Fiber.of_thunk run);
  [%expect
    {|
    1: scheduling timer
    1: timer running
    1: timer done
    2: scheduling timer
    2: timer running
    2: timer done
    3: scheduling timer
    3: timer running
    3: timer done |}]
