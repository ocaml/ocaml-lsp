open Import
open Fiber.O
module Lio = Lev_fiber.Io

type t = Lio.input Lio.t * Lio.output Lio.t

module Io =
  Io.Make
    (struct
      include Fiber

      let raise exn = raise exn
    end)
    (struct
      type input = Lio.Reader.t

      type output = Lio.Writer.t

      let read_line ic =
        let+ res = Lio.Reader.read_line ic in
        match res with
        | Ok s -> Some s
        | Error (`Partial_eof _) -> None

      let read_exactly ic len =
        let+ res = Lio.Reader.read_exactly ic len in
        match res with
        | Ok s -> Some s
        | Error (`Partial_eof _) -> None

      let write oc s =
        Fiber.of_thunk (fun () ->
            Lio.Writer.add_string oc s;
            Fiber.return ())
    end)

let send (_, oc) packets =
  Lio.with_write oc ~f:(fun writer ->
      let* () = Fiber.sequential_iter packets ~f:(Io.write writer) in
      Lio.Writer.flush writer)

let recv (ic, _) = Lio.with_read ic ~f:Io.read

let make ic oc = (ic, oc)

let close (ic, oc) what =
  Fiber.of_thunk (fun () ->
      (match what with
      | `Write -> Lio.close oc
      | `Read -> Lio.close ic);
      Fiber.return ())
