open Stdune
open Fiber.O
module Timer = Lev_fiber.Timer
module Wheel = Timer.Wheel

let%expect_test "sleep" =
  Lev_fiber.run (fun () ->
      print_endline "sleep";
      let+ () = Lev_fiber.Timer.sleepf 0.1 in
      print_endline "awake")
  |> Lev_fiber.Error.ok_exn;
  [%expect {|
    sleep
    awake |}]

let%expect_test "timer wheel start/stop" =
  Lev_fiber.run (fun () ->
      let* wheel = Wheel.create ~delay:10. in
      Fiber.fork_and_join_unit
        (fun () ->
          print_endline "wheel: run";
          Wheel.run wheel)
        (fun () ->
          print_endline "wheel: stop";
          Wheel.stop wheel))
  |> Lev_fiber.Error.ok_exn;
  [%expect {|
    wheel: run
    wheel: stop |}]

let%expect_test "timer wheel cancellation" =
  Lev_fiber.run (fun () ->
      let* wheel = Wheel.create ~delay:10. in
      Fiber.fork_and_join_unit
        (fun () ->
          let* task = Wheel.task wheel in
          let* () = Wheel.cancel task in
          let* result = Wheel.await task in
          match result with
          | `Ok -> assert false
          | `Cancelled ->
              print_endline "cancellation succeeded";
              Wheel.stop wheel)
        (fun () ->
          print_endline "wheel: stop";
          Wheel.run wheel))
  |> Lev_fiber.Error.ok_exn;
  [%expect {|
    cancellation succeeded
    wheel: stop |}]

let%expect_test "wheel - stopping with running timers" =
  Lev_fiber.run (fun () ->
      let* wheel = Wheel.create ~delay:1.0 in
      Fiber.fork_and_join_unit
        (fun () ->
          print_endline "wheel: run";
          Wheel.run wheel)
        (fun () ->
          printfn "creating a task";
          let* task = Wheel.task wheel in
          printfn "stopping the wheel";
          let* () = Wheel.stop wheel in
          let+ res = Wheel.await task in
          match res with
          | `Ok -> assert false
          | `Cancelled -> printfn "timer cancelled"))
  |> Lev_fiber.Error.ok_exn;
  [%expect
    {|
    wheel: run
    creating a task
    stopping the wheel
    timer cancelled |}]

let%expect_test "wheel - reset" =
  Lev_fiber.run (fun () ->
      let delay = 0.1 in
      let* wheel = Wheel.create ~delay in
      let* task = Wheel.task wheel in
      let test () =
        Fiber.fork_and_join_unit
          (fun () ->
            let* () = Lev_fiber.yield () in
            printfn "cancelling task";
            Wheel.cancel task)
          (fun () ->
            let* res = Wheel.await task in
            (match res with
            | `Cancelled -> printfn "cancelled"
            | `Ok -> assert false);
            let* () = Wheel.reset task in
            let* res = Wheel.await task in
            (match res with
            | `Cancelled -> assert false
            | `Ok -> printfn "success after reset");
            Wheel.stop wheel)
      in
      Fiber.fork_and_join_unit (fun () -> Wheel.run wheel) test)
  |> Lev_fiber.Error.ok_exn;
  [%expect {|
    cancelling task
    cancelled
    success after reset |}]

let%expect_test "wheel - set_delay" =
  Lev_fiber.run (fun () ->
      let* wheel = Wheel.create ~delay:200. in
      let* task = Wheel.task wheel in
      let test () =
        let* () = Wheel.set_delay wheel ~delay:0. in
        let* res = Wheel.await task in
        match res with
        | `Cancelled -> assert false
        | `Ok ->
            printfn "immediately finished after delay";
            Wheel.stop wheel
      in
      Fiber.fork_and_join_unit (fun () -> Wheel.run wheel) test)
  |> Lev_fiber.Error.ok_exn;
  [%expect {| immediately finished after delay |}]
