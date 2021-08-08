module Channel = Fiber_unix.Private.Channel

let%expect_test "" =
  let ch = Channel.create () in
  let initial_time = Unix.gettimeofday () in
  let print_time () =
    Unix.gettimeofday () -. initial_time
    |> int_of_float |> string_of_int |> print_endline
  in
  let th0 =
    Thread.create
      (fun () ->
        print_time ();
        for _ = 0 to 1 do
          ignore (Channel.get ch);
          print_time ()
        done)
      ()
  in
  let th1 =
    Thread.create
      (fun () ->
        Unix.sleep 1;
        ignore @@ Channel.send ch ();
        Unix.sleep 2;
        ignore @@ Channel.send ch ())
      ()
  in
  Thread.join th0;
  Thread.join th1;
  [%expect {|
    0
    1
    3 |}]

let%expect_test _ =
  let ch = Channel.create () in
  let print_int i =
    string_of_int i |> print_endline;
    flush_all ()
  in
  let th0 =
    Thread.create
      (fun () ->
        let v0 = Channel.get ch in
        print_int @@ Result.get_ok v0;
        Unix.sleepf 1.;
        let v1 = Channel.get ch in
        print_int @@ Result.get_ok v1)
      ()
  in
  let th1 =
    Thread.create
      (fun () ->
        ignore @@ Channel.send ch 0;
        let removable = Channel.send_removable ch 1 in
        Channel.remove_if_not_consumed @@ Result.get_ok removable;
        Channel.send ch 2)
      ()
  in
  Thread.join th0;
  Thread.join th1;
  [%expect {|
    0
    2 |}]
