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

  let port t ~name ~description =
    ( name
    , Arg.Int
        (fun p ->
          match t.port with
          | Some _ -> raise @@ Arg.Bad "port is already set once"
          | None -> t.port <- Some p)
    , description )
  ;;

  let create () =
    let t =
      { pipe = None; port = None; stdio = false; spec = []; clientProcessId = None }
    in
    let spec =
      [ "--pipe", Arg.String (fun p -> t.pipe <- Some p), "set pipe path"
      ; port t ~name:"--socket" ~description:"set the port"
      ; port t ~name:"--port" ~description:"synonym for --socket"
      ; "--stdio", Arg.Unit (fun () -> t.stdio <- true), "set stdio"
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
  ;;

  let spec t = t.spec
  let clientProcessId t = t.clientProcessId

  let channel { pipe; port; stdio; spec = _; clientProcessId = _ }
    : (Channel.t, string) result
    =
    match pipe, port, stdio with
    | None, None, _ -> Ok Stdio
    | Some p, None, false -> Ok (Pipe p)
    | None, Some s, false -> Ok (Socket s)
    | _, _, _ -> Error "invalid arguments"
  ;;
end

let args ?channel ?clientProcessId () =
  let args =
    match clientProcessId with
    | None -> []
    | Some pid -> [ "--clientProcessId"; string_of_int pid ]
  in
  match (channel : Channel.t option) with
  | None -> args
  | Some Stdio -> "--stdio" :: args
  | Some (Pipe pipe) -> "--pipe" :: pipe :: args
  | Some (Socket port) -> "--socket" :: string_of_int port :: args
;;
