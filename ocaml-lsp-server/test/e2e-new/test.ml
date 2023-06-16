module Import = struct
  include struct
    include Stdune

    module List = struct
      include List

      let find_mapi ~f l =
        let rec k i = function
          | [] -> None
          | x :: xs -> (
            match f i x with
            | Some x' -> Some x'
            | None -> (k [@tailcall]) (i + 1) xs)
        in
        k 0 l

      let take n l =
        let rec take acc n l =
          if n = 0 then acc
          else
            match l with
            | [] -> failwith "list shorter than n"
            | x :: xs -> (take [@tailcall]) (x :: acc) (n - 1) xs
        in
        List.rev (take [] n l)
    end

    module Array = struct
      include Array

      module Iter : sig
        type 'a t

        val create : 'a array -> 'a t

        val has_next : 'a t -> bool

        val next : 'a t -> 'a option

        val next_exn : 'a t -> 'a
      end = struct
        type 'a t =
          { contents : 'a array
          ; mutable ix : int
          }

        let create contents = { contents; ix = 0 }

        let has_next t = t.ix < Array.length t.contents

        let next_exn t =
          let { contents; ix } = t in
          let v = contents.(ix) in
          t.ix <- ix + 1;
          v

        let next t = if has_next t then Some (next_exn t) else None
      end
    end
  end

  include Fiber.O
  module Client = Lsp_fiber.Client
  include Lsp.Types
  module Uri = Lsp.Uri
end

open Import

module T : sig
  val run :
       ?extra_env:string list
    -> ?handler:unit Client.Handler.t
    -> (unit Client.t -> 'a Fiber.t)
    -> 'a
end = struct
  let _PATH =
    Bin.parse_path (Option.value ~default:"" @@ Env.get Env.initial "PATH")

  let bin =
    Bin.which "ocamllsp" ~path:_PATH |> Option.value_exn |> Path.to_string

  let run ?(extra_env = []) ?handler f =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let pid =
      let env =
        let current = Unix.environment () in
        Array.to_list current @ extra_env |> Spawn.Env.of_list
      in
      Spawn.spawn
        ~env
        ~prog:bin
        ~argv:[ bin ]
        ~stdin:stdin_i
        ~stdout:stdout_o
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
    (* TODO replace the wheel once we can cancel sleep *)
    let waitpid wheel =
      let* timeout = Lev_fiber.Timer.Wheel.task wheel in
      Fiber.finalize ~finally:(fun () -> Lev_fiber.Timer.Wheel.stop wheel)
      @@ fun () ->
      let cancelled = ref false in
      Fiber.fork_and_join_unit
        (fun () ->
          let+ timeout = Lev_fiber.Timer.Wheel.await timeout in
          match timeout with
          | `Ok ->
            Unix.kill pid Sys.sigkill;
            cancelled := true
          | `Cancelled -> ())
        (fun () ->
          let* (_ : Unix.process_status) = Lev_fiber.waitpid ~pid in
          if !cancelled then Fiber.return ()
          else Lev_fiber.Timer.Wheel.cancel timeout)
    in
    Lev_fiber.run (fun () ->
        let* wheel = Lev_fiber.Timer.Wheel.create ~delay:3.0 in
        let+ res = init
        and+ () =
          Fiber.all_concurrently_unit
            [ waitpid wheel; Lev_fiber.Timer.Wheel.run wheel ]
        in
        res)
    |> Lev_fiber.Error.ok_exn
end

include T
