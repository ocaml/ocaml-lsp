open Stdune
open Fiber.O
module Io = Lev_fiber.Io
module Session_id = Id.Make ()

module Session = struct
  module Lexer = Csexp.Parser.Lexer
  module Stack = Csexp.Parser.Stack
  module Id = Session_id

  type state =
    | Closed
    | Open of {
        out_channel : Io.output Io.t;
        in_channel : Io.input Io.t;
        read : Fiber.Mutex.t;
        write : Fiber.Mutex.t;
        socket : bool;
      }

  type t = { id : Id.t; mutable state : state }

  let create ~socket in_channel out_channel =
    let id = Id.gen () in
    let state =
      Open
        {
          in_channel;
          out_channel;
          socket;
          read = Fiber.Mutex.create ();
          write = Fiber.Mutex.create ();
        }
    in
    { id; state }

  let close t =
    match t.state with
    | Closed -> ()
    | Open { in_channel; out_channel; read = _; write = _; socket } ->
        (match socket with
        | false -> ()
        | true -> (
            try
              let fd = Io.fd out_channel in
              Unix.shutdown (Lev_fiber.Fd.fd_exn fd) Unix.SHUTDOWN_ALL
            with Unix.Unix_error (_, _, _) -> ()));
        Io.close in_channel;
        Io.close out_channel;
        t.state <- Closed

  let read t =
    match t.state with
    | Closed -> Fiber.return None
    | Open { in_channel; _ } ->
        let lexer = Lexer.create () in
        let buf = Buffer.create 16 in
        let rec loop reader parser =
          match Io.Reader.Expert.available reader with
          | `Eof ->
              Lexer.feed_eoi lexer;
              Fiber.return None
          | `Ok 0 ->
              let* () = Io.Reader.Expert.refill reader in
              loop reader parser
          | `Ok _ -> (
              let token =
                let c = Io.Reader.read_char_exn reader in
                Lexer.feed lexer c
              in
              match token with
              | Atom n ->
                  Buffer.clear buf;
                  atom reader parser n
              | (Lparen | Rparen | Await) as token -> (
                  let parser = Stack.add_token token parser in
                  match parser with
                  | Sexp (sexp, Empty) -> Fiber.return (Some sexp)
                  | parser -> loop reader parser))
        and atom reader parser len =
          if len = 0 then
            let atom = Buffer.contents buf in
            match Stack.add_atom atom parser with
            | Sexp (sexp, Empty) -> Fiber.return (Some sexp)
            | parser -> loop reader parser
          else
            match Io.Reader.Expert.available reader with
            | `Eof ->
                Lexer.feed_eoi lexer;
                Fiber.return None
            | `Ok 0 ->
                let* () = Io.Reader.Expert.refill reader in
                atom reader parser len
            | `Ok _ ->
                let bytes, { Io.Slice.pos; len = buf_len } =
                  Io.Reader.Expert.buffer reader
                in
                let len_read = min len buf_len in
                Buffer.add_subbytes buf bytes pos len_read;
                Io.Reader.Expert.consume reader ~len:len_read;
                atom reader parser (len - len_read)
        in
        Io.with_read in_channel ~f:(fun reader -> loop reader Stack.Empty)

  let read t =
    match t.state with
    | Closed -> Fiber.return None
    | Open { read = mutex; _ } ->
        Fiber.Mutex.with_lock mutex ~f:(fun () -> read t)

  let write_closed sexps =
    Code_error.raise "attempting to write to a closed channel"
      [ ("sexp", Dyn.(list Sexp.to_dyn) sexps) ]

  let write t sexps =
    match t.state with
    | Closed -> write_closed sexps
    | Open { out_channel; _ } ->
        Io.with_write out_channel ~f:(fun writer ->
            let rec write sexp src_pos =
              if src_pos = String.length sexp then Fiber.return ()
              else
                let* size =
                  let size = Io.Writer.Expert.available writer in
                  if size > 0 then Fiber.return size
                  else
                    let+ () = Io.Writer.flush writer in
                    Io.Writer.Expert.available writer
                in
                let dst, { Io.Slice.pos = dst_pos; len } =
                  Io.Writer.Expert.prepare writer ~len:size
                in
                let len = min len (String.length sexp - src_pos) in
                Bytes.blit_string ~src:sexp ~src_pos ~dst ~dst_pos ~len;
                Io.Writer.Expert.commit writer ~len;
                write sexp (src_pos + len)
            in
            let rec loop = function
              | [] -> Io.Writer.flush writer
              | sexp :: sexps ->
                  let* () = write (Csexp.to_string sexp) 0 in
                  loop sexps
            in
            loop sexps)

  let write t sexps =
    match t.state with
    | Closed -> write_closed sexps
    | Open { write = mutex; _ } ->
        Fiber.Mutex.with_lock mutex ~f:(fun () -> write t sexps)
end

let connect fd sockaddr =
  let* () = Lev_fiber.Socket.connect fd sockaddr in
  let+ i, o = Lev_fiber.Io.create_rw fd in
  Session.create ~socket:true i o

let serve server ~f =
  let module Server = Lev_fiber.Socket.Server in
  Server.serve server ~f:(fun session ->
      let* i, o = Server.Session.io session in
      let session = Session.create ~socket:true i o in
      f session)
