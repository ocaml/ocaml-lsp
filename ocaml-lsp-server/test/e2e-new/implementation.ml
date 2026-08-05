open Test.Import

let lib_ml_source =
  "type t = int\n\n\
   module Nested = struct\n\
  \  type t = string\n\
   end\n\n\
   let value = 42\n\n\
   module type S = sig\n\
  \  type t\n\
   end\n\n\
   module Included = struct\n\
  \  type included = bool\n\
   end\n\n\
   include Included\n\n\
   module Alias = Nested\n\n\
   module Outer = struct\n\
  \  module Inner = struct\n\
  \    type t = float\n\
  \  end\n\
   end\n"
;;

let lib_mli_source =
  "type t\n\n\
   module Nested : sig\n\
  \  type t\n\
   end\n\n\
   val value : int\n\n\
   module type S = sig\n\
  \  type t\n\
   end\n\n\
   type included\n\n\
   module Alias : sig\n\
  \  type t\n\
   end\n\n\
   module Outer : sig\n\
  \  module Inner : sig\n\
  \    type t\n\
  \  end\n\
   end\n"
;;

let setup_workspace ?(without_implementation = false) () =
  let dir = Test.temp_dir "ocamllsp-implementation-" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 2.5)\n";
  let dune =
    if without_implementation
    then
      "(library\n (name implementation_files)\n (modules_without_implementation lib))\n"
    else "(library\n (name implementation_files))\n"
  in
  Test.write_file (Filename.concat dir "dune") dune;
  if not without_implementation
  then Test.write_file (Filename.concat dir "lib.ml") lib_ml_source;
  Test.write_file (Filename.concat dir "lib.mli") lib_mli_source;
  Test.run_command ~cwd:dir "dune build";
  dir
;;

let print_locations = Test.print_locations

(* [impl_source] opens lib.ml with the given text instead of relying on the
   file on disk, as a client with unsaved changes would. [on_impl] opens
   lib.ml instead of lib.mli and sends the request for lib.ml. *)
let run_test ?impl_source ?(without_implementation = false) ?(on_impl = false) position =
  let dir = setup_workspace ~without_implementation () in
  let path = Filename.concat dir (if on_impl then "lib.ml" else "lib.mli") in
  let uri = DocumentUri.of_path path in
  let source = Fs_io.read_file path |> Result.ok_exn in
  let stderr = Unix.openfile Test.null_device [ O_WRONLY ] 0 in
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  (Test.run_initialized ~stderr ~handler
   @@ fun client ->
   let textDocument =
     TextDocumentItem.create
       ~uri
       ~languageId:(LanguageKind.Other "ocaml")
       ~version:0
       ~text:source
   in
   let* () =
     Client.notification
       client
       (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
   in
   let* () =
     match impl_source with
     | None -> Fiber.return ()
     | Some text ->
       let impl_uri = DocumentUri.of_path (Filename.concat dir "lib.ml") in
       let impl_document =
         TextDocumentItem.create
           ~uri:impl_uri
           ~languageId:(LanguageKind.Other "ocaml")
           ~version:0
           ~text
       in
       Client.notification
         client
         (TextDocumentDidOpen
            (DidOpenTextDocumentParams.create ~textDocument:impl_document))
   in
   let textDocument = TextDocumentIdentifier.create ~uri in
   let* response =
     Client.request
       client
       (TextDocumentImplementation
          (ImplementationParams.create ~textDocument ~position ()))
   in
   print_locations response;
   let* () = Client.request client Shutdown in
   let* () = Fiber.Ivar.read diagnostics in
   Client.stop client);
  Unix.close stderr
;;

let%expect_test "returns implementation of an interface type" =
  run_test (Position.create ~line:0 ~character:5);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "returns implementation of a nested interface type" =
  run_test (Position.create ~line:3 ~character:7);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 8, "line": 3 },
      "start": { "character": 7, "line": 3 }
    }
    |}]
;;

let%expect_test "returns implementation of a type nested two module levels deep" =
  run_test (Position.create ~line:20 ~character:9);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 10, "line": 22 },
      "start": { "character": 9, "line": 22 }
    }
    |}]
;;

let%expect_test "returns no implementation for a value declaration" =
  run_test (Position.create ~line:6 ~character:5);
  [%expect {| [] |}]
;;

let%expect_test "returns no implementation for a type inside a module type" =
  run_test (Position.create ~line:9 ~character:7);
  [%expect {| [] |}]
;;

let%expect_test "follows types exposed through includes" =
  run_test (Position.create ~line:12 ~character:5);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 15, "line": 13 },
      "start": { "character": 7, "line": 13 }
    }
    |}]
;;

let%expect_test "follows types exposed through module aliases" =
  run_test (Position.create ~line:15 ~character:7);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 8, "line": 3 },
      "start": { "character": 7, "line": 3 }
    }
    |}]
;;

let%expect_test "uses the open implementation when it has unsaved changes" =
  (* The open buffer moves [type t = int] from line 0 to line 4; the response
     must point at the buffer, not at the file on disk. *)
  let impl_source =
    "(* preamble *)\n(* preamble *)\n(* preamble *)\n\n" ^ lib_ml_source
  in
  run_test ~impl_source (Position.create ~line:0 ~character:5);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 4 },
      "start": { "character": 5, "line": 4 }
    }
    |}]
;;

let%expect_test "returns no implementation when there is no implementation file" =
  run_test ~without_implementation:true (Position.create ~line:0 ~character:5);
  [%expect {| [] |}]
;;

let%expect_test "returns no implementation when requested on an implementation file" =
  run_test ~on_impl:true (Position.create ~line:0 ~character:5);
  [%expect {| [] |}]
;;

let%expect_test "returns no implementation when the cursor is on the type keyword" =
  run_test (Position.create ~line:0 ~character:0);
  [%expect {| [] |}]
;;

let%expect_test "returns no implementation when the cursor is on the type's body" =
  run_test (Position.create ~line:0 ~character:9);
  [%expect {| [] |}]
;;

let%expect_test "returns a location when the implementation has a type error" =
  let impl_source = "type t = int\nlet x : string = 1\n" in
  run_test ~impl_source (Position.create ~line:0 ~character:5);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;

let%expect_test "resolves the type even when the implementation does not parse" =
  (* Merlin recovers from the parse error and keeps the type declaration, so
     the query still finds the implementation. *)
  let impl_source = "type t = int\nlet x =\n" in
  run_test ~impl_source (Position.create ~line:0 ~character:5);
  [%expect
    {|
    lib.ml
    {
      "end": { "character": 6, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    |}]
;;
