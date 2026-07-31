open Test.Import

module Mailbox = struct
  type 'a t =
    { mutable pending : 'a list
    ; mutable waiter : 'a Fiber.Ivar.t option
    }

  let create () = { pending = []; waiter = None }

  let push t value =
    match t.waiter with
    | None ->
      t.pending <- t.pending @ [ value ];
      Fiber.return ()
    | Some waiter ->
      t.waiter <- None;
      Fiber.Ivar.fill waiter value
  ;;

  let wait t =
    match t.pending with
    | value :: rest ->
      t.pending <- rest;
      Fiber.return value
    | [] ->
      let waiter = Fiber.Ivar.create () in
      t.waiter <- Some waiter;
      Fiber.Ivar.read waiter
  ;;
end

let started_prefix = "Ocamlformat-RPC server started with PID "

let normalize_started_message message =
  if String.is_prefix message ~prefix:started_prefix
  then started_prefix ^ "<pid>"
  else message
;;

let wait_for_exit pid =
  let rec loop attempts =
    if Int.equal attempts 0
    then failwith "ocamlformat-rpc did not exit"
    else (
      match Unix.kill pid 0 with
      | () ->
        let* () = Lev_fiber.Timer.sleepf 0.01 in
        loop (attempts - 1)
      | exception Unix.Unix_error (Unix.ESRCH, _, _) -> Fiber.return ())
  in
  loop 100
;;

let read_pids file =
  Fs_io.read_file file
  |> Result.ok_exn
  |> String.split_lines
  |> List.filter_map ~f:Int.of_string_opt
;;

let%expect_test "restart ocamlformat-rpc after process exit" =
  let temp = Test.temp_dir "ocamllsp-ocamlformat-restart-" in
  let bin_dir = Filename.concat temp "bin" in
  let pid_file = Filename.concat temp "pids" in
  Unix.mkdir bin_dir 0o700;
  let real = Bin.which "ocamlformat-rpc" |> Option.value_exn in
  let shim = Filename.concat bin_dir "ocamlformat-rpc" in
  Test.write_file
    shim
    "#!/bin/sh\n\
     printf '%s\\n' \"$$\" >> \"$OCAMLFORMAT_PID_FILE\"\n\
     exec \"$REAL_OCAMLFORMAT_RPC\" \"$@\"\n";
  Unix.chmod shim 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf -- " ^ Filename.quote temp) : int))
    (fun () ->
       let started = Mailbox.create () in
       let on_notification _ = function
         | Lsp.Server_notification.LogMessage { message; _ }
           when String.is_prefix message ~prefix:started_prefix ->
           Mailbox.push started message
         | _ -> Fiber.return ()
       in
       let handler = Client.Handler.make ~on_notification () in
       let path = bin_dir ^ ":" ^ Option.value (Sys.getenv_opt "PATH") ~default:"" in
       Test.run_initialized
         ~extra_env:
           [ "PATH=" ^ path
           ; "OCAMLFORMAT_PID_FILE=" ^ pid_file
           ; "REAL_OCAMLFORMAT_RPC=" ^ real
           ]
         ~handler
       @@ fun client ->
       let uri = Uri.of_path (Filename.concat temp "restart.ml") in
       let text = "let map = List.map\n" in
       let* () = Dune_rpc_test.open_document client ~uri ~text in
       let position = Position.create ~line:0 ~character:15 in
       let* first = Hover_helpers.hover ~uri client position in
       let* first_started = Mailbox.wait started in
       print_endline (normalize_started_message first_started);
       Hover_helpers.print_hover first;
       let first_pid = List.hd_exn (read_pids pid_file) in
       Unix.kill first_pid Sys.sigkill;
       let* () = wait_for_exit first_pid in
       let* () = Lev_fiber.Timer.sleepf 0.02 in
       let* second = Hover_helpers.hover ~uri client position in
       let* second_started = Mailbox.wait started in
       print_endline (normalize_started_message second_started);
       Hover_helpers.print_hover second;
       let pids = read_pids pid_file in
       if List.length pids < 2 then failwith "ocamlformat-rpc was not restarted";
       Test.exit_client client);
  [%expect
    {|
    Ocamlformat-RPC server started with PID <pid>
    {
      "contents": {
        "kind": "plaintext",
        "value": "('a -> 'b) -> 'a list -> 'b list\n***\n[map f [a1; ...; an]] applies function [f] to [a1, ..., an],\n   and builds the list [[f a1; ...; f an]]\n   with the results returned by [f]."
      },
      "range": {
        "end": { "character": 18, "line": 0 },
        "start": { "character": 10, "line": 0 }
      }
    }
    Ocamlformat-RPC server started with PID <pid>
    {
      "contents": {
        "kind": "plaintext",
        "value": "('a -> 'b) -> 'a list -> 'b list\n***\n[map f [a1; ...; an]] applies function [f] to [a1, ..., an],\n   and builds the list [[f a1; ...; f an]]\n   with the results returned by [f]."
      },
      "range": {
        "end": { "character": 18, "line": 0 },
        "start": { "character": 10, "line": 0 }
      }
    }
    |}]
;;
