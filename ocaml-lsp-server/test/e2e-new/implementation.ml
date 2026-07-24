open Test.Import

let setup ?(preprocess = "") ~request_file files =
  let disk_source = Stdlib.List.assoc request_file files in
  let source, position = Test.parse_cursor disk_source in
  let dir = Test.temp_dir "implementation" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.0)\n";
  let interface_only =
    List.filter_map files ~f:(fun (file, _) ->
      match Filename.extension file with
      | ".mli" ->
        let name = Filename.remove_extension file in
        if Stdlib.List.mem_assoc (name ^ ".ml") files then None else Some name
      | _ -> None)
  in
  Test.write_file
    (Filename.concat dir "dune")
    (Printf.sprintf
       "(library (name implementation_files) (modules_without_implementation %s) %s)\n"
       (String.concat ~sep:" " interface_only)
       preprocess);
  List.iter files ~f:(fun (file, text) ->
    let text = if file = request_file then source else text in
    Test.write_file (Filename.concat dir file) text);
  Test.run_command ~cwd:dir "dune build";
  dir, source, position
;;

let notifications () =
  let stopped = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | LogTrace { message = "Stopping Merlin configuration process"; _ } ->
           Fiber.Ivar.fill stopped ()
         | _ -> Fiber.return ())
      ()
  in
  handler, stopped
;;

let close client uri =
  Client.notification
    client
    (TextDocumentDidClose { textDocument = TextDocumentIdentifier.create ~uri })
;;

let print_locations dir locations =
  (* Do not hide an incorrect build-tree URI by printing only its basename. *)
  (match locations with
   | Some (`Location locations) ->
     List.iter locations ~f:(fun (location : Location.t) ->
       let path = Uri.to_path location.uri in
       assert (
         Unix.realpath path = Unix.realpath (Filename.concat dir (Filename.basename path))))
   | None -> ()
   | Some (`LocationLink _ | `SingleLocation _) -> assert false);
  Test.print_locations locations
;;

let request client dir uri position =
  let params =
    ImplementationParams.create
      ~textDocument:(TextDocumentIdentifier.create ~uri)
      ~position
      ()
  in
  Fiber.collect_errors (fun () ->
    Client.request client (TextDocumentImplementation params))
  >>= function
  | Ok locations ->
    print_locations dir locations;
    Fiber.return ()
  | Error [ { exn = Jsonrpc.Response.Error.E { code = MethodNotFound; _ }; _ } ] ->
    print_endline "Unsupported";
    Fiber.return ()
  | Error errors -> Fiber.reraise_all errors
;;

let run
      ?(request_file = "lib.mli")
      ?(buffers = [])
      ?request_source
      ?(prepare = fun _ -> ())
      ?(repeat = 1)
      files
  =
  let dir, source, position = setup ~request_file files in
  let source, position =
    match request_source with
    | None -> source, position
    | Some source -> Test.parse_cursor source
  in
  prepare dir;
  let handler, stopped = notifications () in
  let stderr = Unix.openfile Test.null_device [ Unix.O_WRONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close stderr)
    (fun () ->
       Test.run_initialized ~cwd:dir ~stderr ~handler ~trace:Verbose ~timeout:10.
       @@ fun client ->
       let uri = Uri.of_path (Filename.concat dir request_file) in
       let* () = Test.open_document ~client ~uri ~source () in
       let* () =
         Fiber.sequential_iter buffers ~f:(fun (file, source) ->
           let uri = Uri.of_path (Filename.concat dir file) in
           Test.open_document ~client ~uri ~source ())
       in
       let* () =
         List.init repeat ~f:Fun.id
         |> Fiber.sequential_iter ~f:(fun (_ : int) -> request client dir uri position)
       in
       let* () =
         Fiber.sequential_iter buffers ~f:(fun (file, _) ->
           close client (Uri.of_path (Filename.concat dir file)))
       in
       let* () = close client uri in
       (* Wait before Shutdown: server shutdown would conceal a temporary-document
          leak by forcibly stopping the configuration process. *)
       let* () = Fiber.Ivar.read stopped in
       let* () = Client.request client Shutdown in
       Client.stop client)
;;

let%expect_test "type declaration" =
  run [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "nested module" =
  run
    [ "lib.mli", "module M : sig type $t end\n"
    ; "lib.ml", "module M = struct type t = int end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 24, "line": 0 },
      "start": { "character": 23, "line": 0 }
    }
    |}]
;;

let%expect_test "deeply nested module" =
  run
    [ "lib.mli", "module M : sig module N : sig type $t end end\n"
    ; "lib.ml", "module M = struct module N = struct type t = int end end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 42, "line": 0 },
      "start": { "character": 41, "line": 0 }
    }
    |}]
;;

let%expect_test "local include" =
  run
    [ "lib.mli", "type $t\n"
    ; ( "lib.ml"
      , {ocaml|module M = struct type t = int end
include M
|ocaml}
      )
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 24, "line": 0 },
      "start": { "character": 23, "line": 0 }
    }
    |}]
;;

let%expect_test "local module alias" =
  run
    [ "lib.mli", "module Alias : sig type $t end\n"
    ; ( "lib.ml"
      , {ocaml|module M = struct type t = int end
module Alias = M
|ocaml}
      )
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 24, "line": 0 },
      "start": { "character": 23, "line": 0 }
    }
    |}]
;;

let%expect_test "value declaration is not a type declaration" =
  run [ "lib.mli", "val $x : int\n"; "lib.ml", "let x = 1\n" ];
  [%expect {| [] |}]
;;

let%expect_test "module type declaration has no implementation" =
  run
    [ "lib.mli", "module type S = sig type $t end\n"
    ; "lib.ml", "module type S = sig type t end\n"
    ];
  [%expect {| [] |}]
;;

let%expect_test "implementation source is not an interface" =
  run ~request_file:"lib.ml" [ "lib.mli", "type t\n"; "lib.ml", "type $t = int\n" ];
  [%expect {| [] |}]
;;

let%expect_test "cursor on the type keyword" =
  run [ "lib.mli", "$type t\n"; "lib.ml", "type t = int\n" ];
  [%expect {| [] |}]
;;

let%expect_test "cursor in the type body" =
  run [ "lib.mli", "type t = $int\n"; "lib.ml", "type t = int\n" ];
  [%expect {| [] |}]
;;

let%expect_test "interface-only module" =
  run [ "lib.mli", "type $t\n" ];
  [%expect {| [] |}]
;;

let%expect_test "implementation disappears after building" =
  run
    ~prepare:(fun dir -> Sys.remove (Filename.concat dir "lib.ml"))
    [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect {| [] |}]
;;

let%expect_test "implementation cannot be read as a file" =
  run
    ~prepare:(fun dir ->
      let path = Filename.concat dir "lib.ml" in
      Sys.remove path;
      Unix.mkdir path 0o700)
    [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect {| [] |}]
;;

let%expect_test "unsaved implementation contents are used repeatedly" =
  run
    ~repeat:3
    ~buffers:[ "lib.ml", "\n\n\ntype t = int\n" ]
    [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 3 },
      "start": { "character": 5, "line": 3 }
    }
    lib.ml
    {
      "end": { "character": 6, "line": 3 },
      "start": { "character": 5, "line": 3 }
    }
    lib.ml
    {
      "end": { "character": 6, "line": 3 },
      "start": { "character": 5, "line": 3 }
    }
    |}]
;;

let%expect_test "temporary implementations are released after repeated requests" =
  run ~repeat:3 [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "implementation no longer declares the type" =
  run
    ~buffers:[ "lib.ml", "let x = 1\n" ]
    [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect {| [] |}]
;;

let%expect_test "unrelated implementation type error" =
  run
    ~buffers:[ "lib.ml", "type t = int\nlet x : string = 1\n" ]
    [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "unrelated implementation parse error" =
  run
    ~buffers:[ "lib.ml", "type t = int\nlet x =\n" ]
    [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "recovered interface" =
  run
    ~request_source:"type $t\nval x :\n"
    [ "lib.mli", "type $t\n"; "lib.ml", "type t = int\n" ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "named functor result" =
  run
    [ "lib.mli", "module F (X : sig end) : sig type $t end\n"
    ; "lib.ml", "module F (X : sig end) = struct type t = int end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 38, "line": 0 },
      "start": { "character": 37, "line": 0 }
    }
    |}]
;;

let%expect_test "generative functor result" =
  run
    [ "lib.mli", "module F () : sig type $t end\n"
    ; "lib.ml", "module F () = struct type t = int end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 27, "line": 0 },
      "start": { "character": 26, "line": 0 }
    }
    |}]
;;

let%expect_test "curried functor with nested result module" =
  run
    [ "lib.mli", "module F (X : sig end) () : sig module M : sig type $t end end\n"
    ; ( "lib.ml"
      , "module F (X : sig end) () = struct module M = struct type t = int end end\n" )
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 59, "line": 0 },
      "start": { "character": 58, "line": 0 }
    }
    |}]
;;

let%expect_test "functor result depending on its parameter" =
  run
    [ "lib.mli", "module F (X : sig type u end) : sig type $t end\n"
    ; "lib.ml", "module F (X : sig type u end) = struct type t = X.u end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 45, "line": 0 },
      "start": { "character": 44, "line": 0 }
    }
    |}]
;;

let%expect_test "missing functor result type does not find an outer type" =
  run
    ~buffers:[ "lib.ml", "type t = int\nmodule F () = struct let x = 0 end\n" ]
    [ "lib.mli", "module F () : sig type $t end\n"
    ; "lib.ml", "module F () = struct type t = int end\n"
    ];
  [%expect {| [] |}]
;;

let%expect_test "alias to a functor" =
  run
    [ "lib.mli", "module Alias () : sig type $t end\n"
    ; "lib.ml", "module F () = struct type t = int end\nmodule Alias = F\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 27, "line": 0 },
      "start": { "character": 26, "line": 0 }
    }
    |}]
;;

let%expect_test "alias to a cross-file functor" =
  run
    [ "dep.mli", "module F () : sig type t end\n"
    ; "dep.ml", "module F () = struct type t = int end\n"
    ; "lib.mli", "module Alias () : sig type $t end\n"
    ; "lib.ml", "module Alias = Dep.F\n"
    ];
  [%expect
    {|
    dep.ml
    {
      "end": { "character": 27, "line": 0 },
      "start": { "character": 26, "line": 0 }
    }
    |}]
;;

let%expect_test "functor parameter is not a result declaration" =
  run
    [ "lib.mli", "module F (X : sig type $t end) : sig type t end\n"
    ; "lib.ml", "module F (X : sig type t end) = struct type t = X.t end\n"
    ];
  [%expect {| [] |}]
;;

let%expect_test "cross-file include" =
  run
    [ "dep.ml", "type t = int\n"
    ; "dep.mli", "type t\n"
    ; "lib.mli", "type $t\n"
    ; "lib.ml", "include Dep\n"
    ];
  [%expect
    {|
    dep.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "cross-file module alias" =
  run
    [ "dep.ml", "type t = int\n"
    ; "dep.mli", "type t\n"
    ; "lib.mli", "module Alias : sig type $t end\n"
    ; "lib.ml", "module Alias = Dep\n"
    ];
  [%expect
    {|
    dep.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "interface-only dependency is not an implementation" =
  run [ "dep.mli", "type t\n"; "lib.mli", "type $t\n"; "lib.ml", "include Dep\n" ];
  [%expect {| [] |}]
;;

let%expect_test "nested signature include" =
  run
    [ "lib.mli", "module M : sig include sig type $t end end\n"
    ; "lib.ml", "module M = struct type t = int end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 24, "line": 0 },
      "start": { "character": 23, "line": 0 }
    }
    |}]
;;

(* The preprocessor blocks only while reading the closed implementation. FIFOs
   put cancellation/failure after temporary-document acquisition, without sleeps
   or assumptions about how quickly Merlin finishes a request. *)
let run_interrupted mode =
  let dir, source, position =
    setup
      ~request_file:"lib.mli"
      ~preprocess:"(preprocess (action (run sh %{dep:gate-pp.sh} %{input-file})))"
      [ "lib.mli", "type $t\n"
      ; "lib.ml", "type t = int\n"
      ; ( "gate-pp.sh"
        , {|set -eu
case "$(cat "$1")" in
  *"= int"*)
    if [ -n "${IMPLEMENTATION_GATE_ROOT:-}" ]; then
      printf x > "$IMPLEMENTATION_GATE_ROOT/entered"
      read -r reply < "$IMPLEMENTATION_GATE_ROOT/release"
      if [ "$reply" = fail ]; then exit 1; fi
    fi
    ;;
esac
cat "$1"
|}
        )
      ]
  in
  let gate = Filename.concat dir ".gate" in
  Unix.mkdir gate 0o700;
  let entered = Filename.concat gate "entered" in
  let release = Filename.concat gate "release" in
  Unix.mkfifo entered 0o600;
  Unix.mkfifo release 0o600;
  let handler, stopped = notifications () in
  let stderr = Unix.openfile Test.null_device [ Unix.O_WRONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close stderr)
    (fun () ->
       Test.run_initialized
         ~cwd:dir
         ~stderr
         ~handler
         ~trace:Verbose
         ~timeout:10.
         ~extra_env:[ "IMPLEMENTATION_GATE_ROOT=" ^ gate ]
       @@ fun client ->
       let uri = Uri.of_path (Filename.concat dir "lib.mli") in
       let* () = Test.open_document ~client ~uri ~source () in
       let* initialized = Client.initialized client in
       let* () =
         match initialized.capabilities.implementationProvider with
         | None | Some (`Bool false) -> request client dir uri position
         | Some _ ->
           let entered = Unix.openfile entered [ O_RDWR; O_NONBLOCK; O_CLOEXEC ] 0 in
           let release = Unix.openfile release [ O_RDWR; O_NONBLOCK; O_CLOEXEC ] 0 in
           let* entered =
             Lev_fiber.Io.create (Lev_fiber.Fd.create entered (`Non_blocking true)) Input
           in
           Fiber.finalize
             (fun () ->
                let cancel = Fiber.Cancel.create () in
                let+ result, () =
                  Fiber.fork_and_join
                    (fun () ->
                       Client.request_with_cancel
                         client
                         cancel
                         (TextDocumentImplementation
                            (ImplementationParams.create
                               ~textDocument:(TextDocumentIdentifier.create ~uri)
                               ~position
                               ())))
                    (fun () ->
                       let* signal =
                         Lev_fiber.Io.with_read entered ~f:(fun reader ->
                           Lev_fiber.Io.Reader.read_exactly reader 1)
                       in
                       assert (signal = Ok "x");
                       let+ reply =
                         match mode with
                         | `Fail -> Fiber.return "fail\n"
                         | `Cancel ->
                           let* () = Fiber.Cancel.fire cancel in
                           (* Unlike detached trace messages, cancellation is processed
                              in the RPC input loop. Echo acknowledges that loop before
                              the preprocessor is allowed to finish. *)
                           let+ (_ : Lsp.Extension.DebugEcho.Params.t) =
                             Client.request client (DebugEcho { message = "cancelled" })
                           in
                           "continue\n"
                       in
                       assert (
                         Unix.write_substring release reply 0 (String.length reply)
                         = String.length reply))
                in
                match result with
                | `Cancelled -> print_endline "Cancelled"
                | `Ok locations -> print_locations dir locations)
             ~finally:(fun () ->
               Lev_fiber.Io.close entered;
               Unix.close release;
               Fiber.return ())
       in
       let* () = close client uri in
       let* () = Fiber.Ivar.read stopped in
       let* () = Client.request client Shutdown in
       Client.stop client)
;;

let%expect_test "temporary document is released after cancellation" =
  run_interrupted `Cancel;
  [%expect {| Cancelled |}]
;;

let%expect_test "temporary document is released after preprocessing fails" =
  run_interrupted `Fail;
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "nested module in an anonymous module type expression" =
  run
    [ ( "lib.mli"
      , "module M : module type of struct module N = struct type $t = int end end\n" )
    ; "lib.ml", "module M = struct module N = struct type t = int end end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 42, "line": 0 },
      "start": { "character": 41, "line": 0 }
    }
    |}]
;;

let%expect_test "anonymous module type expression" =
  run
    [ "lib.mli", "module M : module type of struct type $t = int end\n"
    ; "lib.ml", "module M = struct type t = int end\n"
    ];
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 24, "line": 0 },
      "start": { "character": 23, "line": 0 }
    }
    |}]
;;
