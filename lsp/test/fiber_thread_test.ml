open! Stdune
open Lsp

let s = Scheduler.create ()

let worker = Scheduler.create_thread s

let fb () =
  Scheduler.async worker (fun () ->
      Thread.delay 1.0;
      print_endline "pre epmtive thread finished")

let _ =
  Thread.create
    (fun () ->
      Thread.delay 3.0;
      print_endline "unexpected termination";
      exit 1)
    ()

let () =
  let open Fiber.O in
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
    let+ res = Fiber.fork_and_join (fun () -> first) (fun () -> last) in
    match res with
    | Error `Cancelled, Ok (true, diff) ->
      Format.eprintf "first scheduled cancelled. second one ran after %f@.%!"
        diff;
      Ok ()
    | _, _ -> Error ()
  in
  let all = Fiber.fork_and_join fb timers_finished in
  let res = Scheduler.run s all in
  match res with
  | Ok (), Ok () -> print_endline "finished successfully"
  | _, _ -> Code_error.raise "unexpected error" []
