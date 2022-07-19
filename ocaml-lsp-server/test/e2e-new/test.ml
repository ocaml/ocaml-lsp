module Import = struct
  include Stdune
  include Fiber.O
  module Client = Lsp_fiber.Client
  include Lsp.Types
end

open Import

module T : sig
  val run :
    ?handler:unit Client.Handler.t -> (unit Client.t -> unit Fiber.t) -> unit
end = struct
  let _PATH =
    Bin.parse_path (Option.value ~default:"" @@ Env.get Env.initial "PATH")

  let bin =
    Bin.which "ocamllsp" ~path:_PATH |> Option.value_exn |> Path.to_string

  let env = Spawn.Env.of_list [ "OCAMLLSP_TEST=true" ]

  let run ?handler f =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let pid =
      Spawn.spawn ~env ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o
        ()
    in
    Unix.close stdin_i;
    Unix.close stdout_o;
    let handler =
      match handler with
      | Some h -> h
      | None -> Client.Handler.make ()
    in
    let init =
      let blockity =
        if Sys.win32 then `Blocking
        else (
          Unix.set_nonblock stdout_i;
          Unix.set_nonblock stdin_o;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* in_ = make stdout_i Input in
      let* out = make stdin_o Output in
      let io = Lsp_fiber.Fiber_io.make in_ out in
      let client = Client.make handler io () in
      f client
    in
    let waitpid =
      let+ (_ : Unix.process_status) = Lev_fiber.waitpid ~pid in
      ()
    in
    Lev_fiber.run (fun () -> Fiber.all_concurrently_unit [ init; waitpid ])
    |> Lev_fiber.Error.ok_exn
end

include T
