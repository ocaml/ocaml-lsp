open! Stdune
open Lsp

let s = Scheduler.create ()

let worker = Scheduler.create_thread s

let fb () =
  Scheduler.async worker (fun () ->
      Thread.delay 1.0;
      print_endline "preemptive thread finished")
  |> Scheduler.await_no_cancel

let _ =
  Thread.create
    (fun () ->
      Thread.delay 3.0;
      print_endline "unexpected termination";
      exit 1)
    ()

exception Timer_error

let () =
  let open Fiber.O in
  let detached = Fiber_detached.create () in
  let timers_finished () =
    let timer = Scheduler.create_timer s ~delay:1.0 in
    let now = Unix.gettimeofday () in
    let diff () = Unix.gettimeofday () -. now in
    let first =
      Scheduler.schedule timer (fun () -> Fiber.return (false, diff ()))
    in
    let last =
      Scheduler.schedule timer (fun () -> Fiber.return (true, diff ()))
    in
    let* res = Fiber.fork_and_join (fun () -> first) (fun () -> last) in
    match res with
    | Error `Cancelled, Ok (true, diff) ->
      Format.eprintf "first scheduled cancelled. second one ran after %f@.%!"
        diff;
      let* () =
        Fiber_detached.task detached ~f:(fun () ->
            let+ res = Scheduler.schedule timer (fun () -> Fiber.return ()) in
            match res with
            | Error `Cancelled -> assert false
            | Ok () -> ())
      in
      let+ () = Fiber_detached.stop detached in
      Ok ()
    | _, _ -> Fiber.return (Error Timer_error)
  in
  let all =
    Fiber.parallel_map
      [ fb
      ; timers_finished
      ; (fun () ->
          let+ () = Fiber_detached.run detached in
          Ok ())
      ]
      ~f:(fun f -> f ())
  in
  let res = Scheduler.run s all in
  match res with
  | [ Ok (); Ok (); Ok () ] -> print_endline "finished successfully"
  | _ -> Code_error.raise "unexpected error" []
