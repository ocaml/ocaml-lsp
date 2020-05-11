open! Stdune
open Lsp

let s = Scheduler.create ()

let worker = Scheduler.create_thread s

let fb = Scheduler.async worker (fun () -> Thread.delay 1.0; print_endline "finit")

let () =
  let res = Scheduler.run s fb in
  match res with
  | Ok () -> print_endline "finished"
  | Error _ -> Code_error.raise "unexpected error" []
