open Stdune
open Fiber.O
open Lev_fiber

let%expect_test "create thread" =
  let f () =
    let* thread = Thread.create () in
    let* task = Thread.task thread ~f:(fun () -> print_endline "in thread") in
    let+ result = Thread.await task in
    match result with Error _ -> assert false | Ok () -> Thread.close thread
  in
  run f;
  [%expect {| in thread |}]

let%expect_test "cancellation" =
  let f () =
    let* thread = Thread.create () in
    let keep_running = Atomic.make false in
    let* task =
      Thread.task thread ~f:(fun () ->
          while Atomic.get keep_running do
            ()
          done)
    in
    let* to_cancel = Thread.task thread ~f:(fun () -> assert false) in
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
  run f;
  [%expect {| Successful cancellation |}]
