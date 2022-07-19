open Stdune
open Fiber.O

let thread_yield () = Thread.yield ()

open Lev_fiber

let%expect_test "create thread" =
  let f () =
    let* thread = Thread.create () in
    let task =
      match Thread.task thread ~f:(fun () -> print_endline "in thread") with
      | Ok s -> s
      | Error _ -> assert false
    in
    let+ result = Thread.await task in
    match result with Error _ -> assert false | Ok () -> Thread.close thread
  in
  run f |> Error.ok_exn;
  [%expect {| in thread |}]

let%expect_test "cancellation" =
  let f () =
    let* thread = Thread.create () in
    let keep_running = Atomic.make false in
    let task =
      match
        Thread.task thread ~f:(fun () ->
            while Atomic.get keep_running do
              thread_yield ()
            done)
      with
      | Ok s -> s
      | Error _ -> assert false
    in
    let to_cancel =
      match Thread.task thread ~f:(fun () -> assert false) with
      | Ok task -> task
      | Error _ -> assert false
    in
    let* () = Thread.cancel to_cancel in
    let* res = Thread.await to_cancel in
    (match res with
    | Error `Cancelled ->
        printfn "Successful cancellation";
        Atomic.set keep_running false
    | Ok _ | Error (`Exn _) -> assert false);
    let+ res = Thread.await task in
    (match res with Ok () -> () | Error _ -> assert false);
    Thread.close thread
  in
  run f |> Error.ok_exn;
  [%expect {| Successful cancellation |}]

let%expect_test "deadlock" =
  let f () =
    let+ _ = Fiber.never in
    ()
  in
  try
    run f |> Error.ok_exn;
    assert false
  with Code_error.E e ->
    print_endline e.message;
    [%expect {| Deadlock |}]
