open! Import
open Fiber.O
module S = Fiber_unix.Scheduler

let test f =
  let f =
    Fiber.with_error_handler f ~on_error:(fun exn ->
        Format.printf "%a@." Exn_with_backtrace.pp_uncaught exn;
        Exn_with_backtrace.reraise exn)
  in
  S.run f

let%expect_test "scheduler starts and runs a fiber" =
  S.run
    (Fiber.of_thunk (fun () ->
         print_endline "running";
         Fiber.return ()));
  [%expect {|
    running |}]

let%expect_test "run an async task and wait it for it to finish" =
  let run () =
    let* th = S.create_thread () in
    let async () =
      S.async_exn th (fun () -> print_endline "running in a different thread")
    in
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
  S.run (Fiber.of_thunk run);
  [%expect
    {|
    running in scheduler
    running in a different thread
    finished running task
    stopped thread |}]

let%expect_test "create timer & schedule task" =
  let run () =
    let* timer = S.create_timer ~delay:0.001 in
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
  test run;
  [%expect {|
    scheduling timer
    timer running
    timer done |}]

let%expect_test "create timer & schedule task & cancel it" =
  let run () =
    let* timer = S.create_timer ~delay:3.0 in
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
  test run;
  [%expect {|
    scheduling timer
    timer cancelled successfully |}]

let%expect_test "create multiple timers" =
  let timer () = S.create_timer ~delay:0.001 in
  let run () =
    Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun i ->
        let* timer = timer () in
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
  test run;
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

let%expect_test "create multiple timers" =
  let timer () = S.create_timer ~delay:0.001 in
  let run () =
    let counter = ref 0 in
    let+ () =
      Fiber.parallel_iter [ 1; 2; 3 ] ~f:(fun _ ->
          let* timer = timer () in
          let+ res =
            S.schedule timer (fun () ->
                printf "timer. ";
                Fiber.return ())
          in
          match res with
          | Error `Cancelled -> assert false
          | Ok () -> incr counter)
    in
    printf "counter: %d\n" !counter
  in
  test run;
  [%expect {|
    timer. timer. timer. counter: 3 |}]

let%expect_test "tests rescheduling" =
  let run () =
    let* timer = S.create_timer ~delay:0.05 in
    let counter = ref 0 in
    let+ () =
      Fiber.parallel_iter [ 1; 2; 3 ] ~f:(fun _ ->
          let+ res =
            S.schedule timer (fun () ->
                printf "timer. ";
                Fiber.return ())
          in
          match res with
          | Error `Cancelled -> printf "cancel. "
          | Ok () -> incr counter)
    in
    printf "counter: %d\n" !counter
  in
  test run;
  [%expect {| cancel. cancel. timer. counter: 1 |}]

let%expect_test "detached + timer" =
  let detached = Fiber.Pool.create () in
  let run () =
    let* timer = S.create_timer ~delay:0.05 in
    Fiber.fork_and_join_unit
      (fun () ->
        let* () =
          Fiber.Pool.task detached ~f:(fun () ->
              let* res =
                S.schedule timer (fun () ->
                    print_endline "inside timer";
                    Fiber.return ())
              in
              match res with
              | Ok () -> Fiber.return (print_endline "timer finished")
              | Error `Cancelled -> assert false)
        in
        Fiber.Pool.stop detached)
      (fun () -> Fiber.Pool.run detached)
  in
  test run;
  [%expect {|
    inside timer
    timer finished |}]

let%expect_test "multiple timers" =
  let open Fiber.O in
  let timer delay =
    let+ timer = S.create_timer ~delay in
    (delay, timer)
  in
  let run () =
    let* timers = Fiber.parallel_map [ 0.3; 0.2; 0.1 ] ~f:timer in
    Fiber.parallel_iter timers ~f:(fun (delay, timer) ->
        let+ res =
          S.schedule timer (fun () ->
              printf "timer %.1f\n" delay;
              Fiber.return ())
        in
        match res with
        | Error `Cancelled -> assert false
        | Ok () -> ())
  in
  test run;
  [%expect {|
    timer 0.1
    timer 0.2
    timer 0.3 |}]

let%expect_test "run process" =
  let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
  let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
  let stderr_i, stderr_o = Unix.pipe ~cloexec:true () in
  Unix.close stdin_o;
  let pid =
    Stdune.Pid.of_int
      (Unix.create_process "echo" [| "echo"; "foo" |] stdin_i stdout_o stderr_o)
  in
  Unix.close stdin_i;
  Unix.close stdout_o;
  Unix.close stderr_o;
  let stdout, stderr =
    let stdout_in = Unix.in_channel_of_descr stdout_i in
    let stderr_in = Unix.in_channel_of_descr stderr_i in
    let stdout = Stdune.Io.read_all stdout_in in
    let stderr = Stdune.Io.read_all stderr_in in
    (stdout, stderr)
  in
  let run () =
    let+ res = S.wait_for_process pid in
    print_endline ("stdout: " ^ stdout);
    print_endline ("stderr: " ^ stderr);
    printf "code: %s"
      (match res with
      | Unix.WEXITED n -> string_of_int n
      | _ -> "<signal>")
  in
  test run;
  [%expect {|
    stdout: foo

    stderr:
    code: 0 |}]
