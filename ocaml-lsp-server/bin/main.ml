open Core
open Async

module Channel = struct
  type t = Lsp.Cli.Channel.t =
    | Stdio
    | Pipe of string
    | Socket of int
end

let command =
  Command.async
    ~summary:"OCaml LSP"
    (let%map_open.Command () = return ()
     and channel =
       choose_one_non_optional
         [ flag
             "stdio"
             (no_arg_required Channel.Stdio)
             ~doc:"Communicate over stdio (default)"
         ; flag
             "port"
             (required int |> map_flag ~f:(fun port -> Channel.Socket port))
             ~doc:"PORT Communicate over an internet socket (127.0.0.1:PORT)"
         ; flag
             "unix"
             (required string |> map_flag ~f:(fun unix -> Channel.Pipe unix))
             ~doc:"UNIX Communicate over a unix domain socket"
         ]
         ~if_nothing_chosen:(Default_to Stdio)
     and client_pid =
       flag
         "client-pid"
         (optional int)
         ~doc:
           "PID Process id of client. When passed, the server will exit when this client \
            dies."
     and client_existence_check_interval =
       flag_optional_with_default_doc
         "check-client-existence-every"
         Time_ns.Span.arg_type
         [%sexp_of: Time_ns.Span.t]
         ~default:(Time_ns.Span.of_int_sec 10)
         ~doc:
           "SPAN How frequently to check if client exists. Only relevant with \
            [-client-pid]."
     and dot_merlin =
       flag
         "dot-merlin"
         (optional Filename_unix.arg_type)
         ~doc:"FILE Path to .merlin file. Used for OCaml_plugin."
     in
     fun () ->
       Option.iter client_pid ~f:(fun pid ->
         Clock_ns.every' client_existence_check_interval (fun () ->
           match Signal_unix.can_send_to (Pid.of_int pid) with
           | true -> return ()
           | false -> exit 0));
       (* TODO: Once Jenga is no longer supported, we can explore communicating with dune
          directly instead of reading dot-merlin files. *)
       Ocaml_lsp_server.run channel ~dot_merlin)
;;

let () = Command_unix.run command
