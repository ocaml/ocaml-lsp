module Barrier = Fiber_unix.Private.Barrier

let () = Printexc.record_backtrace false

let print_result x =
  print_endline
    ( match x with
    | Ok () -> "ok"
    | Error (`Closed (`Read b)) -> Printf.sprintf "closed with read: %b" b
    | Error `Timeout -> "timeout" )

let%expect_test "create & close" =
  let b = Barrier.create () in
  Barrier.close b;
  [%expect {||}]

let%expect_test "write" =
  let b = Barrier.create () in
  Barrier.signal b;
  Barrier.close b;
  [%expect {||}]

let%expect_test "timeout" =
  let b = Barrier.create () in
  print_result (Barrier.await b ~timeout:0.1);
  Barrier.close b;
  [%expect {|
    timeout |}]

let%expect_test "read and write" =
  let b = Barrier.create () in
  for _ = 1 to 5 do
    Barrier.signal b
  done;
  print_result (Barrier.await b ~timeout:0.1);
  print_result (Barrier.await b ~timeout:0.1);
  Barrier.close b;
  [%expect {|
    ok
    timeout |}]

let%expect_test "read/write subsequent" =
  let b = Barrier.create () in
  for _ = 1 to 5 do
    Barrier.signal b;
    print_result (Barrier.await b ~timeout:0.1)
  done;
  Barrier.close b;
  [%expect {|
    ok
    ok
    ok
    ok
    ok |}]

let%expect_test "await after close" =
  let b = Barrier.create () in
  Barrier.close b;
  print_result (Barrier.await b ~timeout:0.1);
  [%expect {| closed with read: false |}]

let%expect_test "write after close" =
  let b = Barrier.create () in
  Barrier.close b;
  Barrier.signal b;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ("Unix.Unix_error(Unix.EBADF, \"write\", \"\")") |}]

let%expect_test "signal then close" =
  let b = Barrier.create () in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        Unix.sleepf 0.5;
        Barrier.close b)
      ()
  in
  print_result (Barrier.await b ~timeout:10.0);
  [%expect {| closed with read: false |}]
