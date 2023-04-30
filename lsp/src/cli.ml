module Channel = struct
  type t =
    | Stdio
    | Pipe of string
    | Socket of int
end

module Arg = struct
  type t =
    { mutable pipe : string option
    ; mutable port : int option
    ; mutable stdio : bool
    ; mutable spec : (string * Arg.spec * string) list
    ; mutable clientProcessId : int option
    }

  let create () =
    let t =
      { pipe = None
      ; port = None
      ; stdio = false
      ; spec = []
      ; clientProcessId = None
      }
    in
    let spec =
      [ ("--pipe", Arg.String (fun p -> t.pipe <- Some p), "set pipe path")
      ; ("--socket", Arg.Int (fun p -> t.port <- Some p), "set port")
      ; ("--stdio", Arg.Unit (fun () -> t.stdio <- true), "set stdio")
      ; ( "--node-ipc"
        , Arg.Unit (fun () -> raise @@ Arg.Bad "node-ipc isn't supported")
        , "not supported" )
      ; ( "--clientProcessId"
        , Arg.Int (fun pid -> t.clientProcessId <- Some pid)
        , "set the pid of the lsp client" )
      ]
    in
    t.spec <- spec;
    t

  let spec t = t.spec

  let clientProcessId t = t.clientProcessId

  let read { pipe; port; stdio; spec = _; clientProcessId = _ } :
      (Channel.t, string) result =
    match (pipe, port, stdio) with
    | None, None, _ -> Ok Stdio
    | Some p, None, false -> Ok (Pipe p)
    | None, Some s, false -> Ok (Socket s)
    | _, _, _ -> Error "invalid arguments"
end

let args ?channel ?clientProcessId () =
  let args =
    match clientProcessId with
    | None -> []
    | Some pid -> ["--clientPorcessId"; string_of_int pid]
  in
  match channel with
  | None -> args
  | Some Stdio -> "--stdio" :: args
  | Some (Pipe pipe) -> "--pipe" :: pipe :: args
  | Some (Socket port) -> "--socket" :: socket :: args
