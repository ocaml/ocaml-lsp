open Import

type state =
  | Binary_not_found
  | Out_of_date
  | Running

type t =
  { scheduler : Scheduler.t
  ; state : state Fiber.Ivar.t
  }

let create scheduler = { scheduler; state = Fiber.Ivar.create () }

let state t : state Fiber.t =
  let open Fiber.O in
  let* state = Fiber.Ivar.peek t.state in
  match state with
  | Some s -> Fiber.return s
  | None -> (
    match Bin.which "dune" with
    | None ->
      let res = Binary_not_found in
      let+ () = Fiber.Ivar.fill t.state res in
      res
    | Some bin ->
      let bin = Fpath.to_string bin in
      let dev_null () = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o666 in
      let stdin = dev_null () in
      let stdout = dev_null () in
      let stderr = dev_null () in

      let open Fiber.O in
      let pid =
        let args = Array.of_list [ bin; "rpc"; "--help=plain" ] in
        Unix.create_process bin args stdin stdout stderr |> Stdune.Pid.of_int
      in
      let* status = Scheduler.wait_for_process t.scheduler pid in
      let state =
        match status with
        (* TODO actually turn on *)
        | Unix.WEXITED 0 -> Running
        | _ -> Out_of_date
      in
      let+ () = Fiber.Ivar.fill t.state state in
      state)
