open Stdune
open Fiber.O

let%expect_test "wait for simple process" =
  let stdin, stdin_w = Unix.pipe () in
  let stdout_r, stdout = Unix.pipe () in
  let stderr_r, stderr = Unix.pipe () in
  Unix.close stdin_w;
  Unix.close stdout_r;
  Unix.close stderr_r;
  Lev_fiber.run (fun () ->
      let pid = Unix.create_process "true" [| "true" |] stdin stdout stderr in
      let+ status = Lev_fiber.waitpid ~pid in
      match status with
      | WEXITED n -> printfn "status: %d" n
      | _ -> assert false)
  |> Lev_fiber.Error.ok_exn;
  [%expect {| status: 0 |}]
