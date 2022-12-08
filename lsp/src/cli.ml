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
    }

  let create () =
    let t = { pipe = None; port = None; stdio = false; spec = [] } in
    let spec =
      [ ("--pipe", Arg.String (fun p -> t.pipe <- Some p), "set pipe path")
      ; ("--socket", Arg.Int (fun p -> t.port <- Some p), "set port")
      ; ("--stdio", Arg.Unit (fun () -> t.stdio <- true), "set stdio")
      ; ( "--node-ipc"
        , Arg.Unit (fun () -> raise @@ Arg.Bad "node-ipc isn't supported")
        , "not supported" )
      ]
    in
    t.spec <- spec;
    t

  let spec t = t.spec

  let read { pipe; port; stdio; spec = _ } : (Channel.t, string) result =
    match (pipe, port, stdio) with
    | None, None, _ -> Ok Stdio
    | Some p, None, false -> Ok (Pipe p)
    | None, Some s, false -> Ok (Socket s)
    | _, _, _ -> Error "invalid arguments"
end
