open Test.Import

let a_bin_dune =
  {workspace_symbol|(executable
 (name main)
 (package main)
 (flags :standard -w -32)
 (public_name main)
 (libraries lib))
|workspace_symbol}
;;

let a_bin_a_ml =
  {workspace_symbol|let a_x = 5

module A_B = struct
  type a_b_t = string
  let a_b = "hello"
end

let a_d =
  match "" with
  | "" -> true
  | _ -> false

type user =
  | Admin
  | NotAdmin

let a_u = Admin

let a_arr = []

let a_m, a_n = (1, 2)

let a_i =
  let a_i_h = 6 in
  a_i_h

module StringMap = Map.Make (String)
module My_string = String

module A_Mod : sig
  type t = int

  val compare : t -> t -> int
end = struct
  type t = int

  let private_mod_fn = Stdlib.abs

  let compare = Stdlib.compare
end

module type X_int = sig
  val x : int
end

module Increment (M : X_int) = struct
  let increment_x = M.x + 1
end

exception Foo of string

class stack_of_ints =
  object
    val mutable the_list : int list = []

    method push x = the_list <- x :: the_list

    method pop =
      let result = List.hd the_list in
      the_list <- List.tl the_list;
      result

    method peek = List.hd the_list

    method size = List.length the_list
  end
|workspace_symbol}
;;

let a_bin_main_ml =
  {workspace_symbol|let main_y = Lib.lib_x
;;

let () = 
  let main_z = "test" in

print_endline (main_z);;

print_endline (string_of_int main_y)

;;
print_endline (string_of_int A.a_x)

;;
print_endline (string_of_int (Lib.length []))
|workspace_symbol}
;;

let a_lib_dune =
  {workspace_symbol|(library
 (public_name lib)
 (modules_without_implementation libTypes)
 (flags :standard -w -32-38-27-34)
 (name lib))


(copy_files# ../vendor/*.ml{,i})
|workspace_symbol}
;;

let a_lib_ml =
  {workspace_symbol|include List


type user = {
  name: string
}
let lib_x =  1

let hd = List.hd

let lib_private_fn s = print_endline s

let lib_type: LibTypes.t = "lib_types"

let vendored_x = Vendored_lib.x
|workspace_symbol}
;;

let a_lib_mli =
  {workspace_symbol|include module type of List

val lib_x : int
|workspace_symbol}
;;

let a_lib_types_mli =
  {workspace_symbol|type t = string
|workspace_symbol}
;;

let a_vendor_ml =
  {workspace_symbol|let x = 5
|workspace_symbol}
;;

let b_dune =
  {workspace_symbol|(library
 (flags :standard -w -32-38-27)
 (name lib))
|workspace_symbol}
;;

let b_main_ml =
  {workspace_symbol|let workspace_B = "workspace_B"
|workspace_symbol}
;;

let mkdir path = if not (Stdlib.Sys.file_exists path) then Unix.mkdir path 0o700

type workspace =
  { name : string
  ; path : string
  ; folder : WorkspaceFolder.t
  }

let create_workspace root name =
  let path = Stdlib.Filename.concat root name in
  mkdir path;
  let uri = DocumentUri.of_path path in
  { name; path; folder = WorkspaceFolder.create ~name ~uri }
;;

let setup_workspaces () =
  let root = Test.temp_dir "ocamllsp-workspace-symbol-" in
  let workspace_a = create_workspace root "workspace_symbol_A" in
  let workspace_b = create_workspace root "workspace_symbol_B" in
  let write workspace rel content =
    Test.write_file (Stdlib.Filename.concat workspace.path rel) content
  in
  mkdir (Stdlib.Filename.concat workspace_a.path "bin");
  mkdir (Stdlib.Filename.concat workspace_a.path "lib");
  mkdir (Stdlib.Filename.concat workspace_a.path "vendor");
  write workspace_a "dune-project" "(lang dune 2.5)\n";
  write workspace_a "lib.opam" "";
  write workspace_a "main.opam" "";
  write workspace_a "bin/dune" a_bin_dune;
  write workspace_a "bin/a.ml" a_bin_a_ml;
  write workspace_a "bin/main.ml" a_bin_main_ml;
  write workspace_a "lib/dune" a_lib_dune;
  write workspace_a "lib/lib.ml" a_lib_ml;
  write workspace_a "lib/lib.mli" a_lib_mli;
  write workspace_a "lib/LibTypes.mli" a_lib_types_mli;
  write workspace_a "vendor/vendored_lib.ml" a_vendor_ml;
  write workspace_b "dune-project" "(lang dune 2.5)\n";
  write workspace_b "dune" b_dune;
  write workspace_b "main.ml" b_main_ml;
  workspace_a, workspace_b
;;

let build_project workspace =
  Test.run_command ~cwd:workspace.path "dune build --root . @check"
;;

let clean_project workspace = Test.run_command ~cwd:workspace.path "dune clean"

let kind_to_int kind =
  match SymbolKind.yojson_of_t kind with
  | `Int i -> i
  | json -> failwith ("unexpected symbol kind " ^ Yojson.Safe.to_string json)
;;

let to_test_result workspaces (symbol : SymbolInformation.t) =
  let location = symbol.location in
  let range = location.range in
  let path = DocumentUri.to_path location.uri in
  let workspace_path =
    List.find_map workspaces ~f:(fun workspace ->
      if Stdlib.String.starts_with ~prefix:workspace.path path
      then Some workspace.path
      else None)
  in
  let relative_path =
    match workspace_path with
    | None -> path
    | Some workspace_path ->
      let parent = Stdlib.Filename.dirname workspace_path in
      String.drop path (String.length parent)
  in
  Printf.sprintf
    "%s %d %s %d:%d %d:%d"
    symbol.name
    (kind_to_int symbol.kind)
    relative_path
    range.start.line
    range.start.character
    range.end_.line
    range.end_.character
;;

let print_symbols workspaces symbols =
  let symbols = Option.value symbols ~default:[] in
  List.iter symbols ~f:(fun symbol -> print_endline (to_test_result workspaces symbol))
;;

let workspace_symbol client query =
  Client.request client (WorkspaceSymbol (WorkspaceSymbolParams.create ~query ()))
;;

let run ?on_notification workspaces f =
  let handler = Client.Handler.make ?on_notification () in
  Test.run ~handler (fun client ->
    let run_client () =
      Test.start_client
        ~workspaceFolders:
          (Some (List.map workspaces ~f:(fun workspace -> workspace.folder)))
        client
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let* () = f client in
      let* () = Client.request client Shutdown in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run)
;;

let%expect_test "returns all symbols from workspace" =
  let workspace_a, _workspace_b = setup_workspaces () in
  build_project workspace_a;
  run [ workspace_a ] (fun client ->
    let* symbols = workspace_symbol client "" in
    print_symbols [ workspace_a ] symbols;
    Fiber.return ());
  [%expect
    {|
    stack_of_ints 5 /workspace_symbol_A/bin/a.ml 51:0 65:5
    size 6 /workspace_symbol_A/bin/a.ml 64:4 64:38
    peek 6 /workspace_symbol_A/bin/a.ml 62:4 62:34
    pop 6 /workspace_symbol_A/bin/a.ml 57:4 60:12
    push 6 /workspace_symbol_A/bin/a.ml 55:4 55:45
    the_list 12 /workspace_symbol_A/bin/a.ml 53:4 53:40
    Foo 9 /workspace_symbol_A/bin/a.ml 49:0 49:23
    Increment 2 /workspace_symbol_A/bin/a.ml 45:0 47:3
    increment_x 12 /workspace_symbol_A/bin/a.ml 46:2 46:27
    X_int 2 /workspace_symbol_A/bin/a.ml 41:0 43:3
    x 12 /workspace_symbol_A/bin/a.ml 42:2 42:13
    A_Mod 2 /workspace_symbol_A/bin/a.ml 29:0 39:3
    compare 12 /workspace_symbol_A/bin/a.ml 38:2 38:30
    private_mod_fn 12 /workspace_symbol_A/bin/a.ml 36:2 36:33
    t 15 /workspace_symbol_A/bin/a.ml 34:2 34:14
    My_string 2 /workspace_symbol_A/bin/a.ml 27:0 27:25
    StringMap 2 /workspace_symbol_A/bin/a.ml 26:0 26:36
    a_i 12 /workspace_symbol_A/bin/a.ml 22:0 24:7
    a_arr 12 /workspace_symbol_A/bin/a.ml 18:0 18:14
    a_u 12 /workspace_symbol_A/bin/a.ml 16:0 16:15
    user 15 /workspace_symbol_A/bin/a.ml 12:0 14:12
    NotAdmin 9 /workspace_symbol_A/bin/a.ml 14:2 14:12
    Admin 9 /workspace_symbol_A/bin/a.ml 13:2 13:9
    a_d 12 /workspace_symbol_A/bin/a.ml 7:0 10:14
    A_B 2 /workspace_symbol_A/bin/a.ml 2:0 5:3
    a_b 12 /workspace_symbol_A/bin/a.ml 4:2 4:19
    a_b_t 15 /workspace_symbol_A/bin/a.ml 3:2 3:21
    a_x 12 /workspace_symbol_A/bin/a.ml 0:0 0:11
    main_y 12 /workspace_symbol_A/bin/main.ml 0:0 0:22
    vendored_x 12 /workspace_symbol_A/lib/lib.ml 14:0 14:31
    lib_type 12 /workspace_symbol_A/lib/lib.ml 12:0 12:38
    lib_private_fn 12 /workspace_symbol_A/lib/lib.ml 10:0 10:38
    hd 12 /workspace_symbol_A/lib/lib.ml 8:0 8:16
    lib_x 12 /workspace_symbol_A/lib/lib.ml 6:0 6:14
    user 15 /workspace_symbol_A/lib/lib.ml 3:0 5:1
    name 7 /workspace_symbol_A/lib/lib.ml 4:2 4:14
    t 15 /workspace_symbol_A/lib/LibTypes.mli 0:0 0:15
    x 12 /workspace_symbol_A/vendor/vendored_lib.ml 0:0 0:9
    |}]
;;

let%expect_test "returns filtered symbols from workspace" =
  let workspace_a, _workspace_b = setup_workspaces () in
  build_project workspace_a;
  run [ workspace_a ] (fun client ->
    let* symbols = workspace_symbol client "a_" in
    print_symbols [ workspace_a ] symbols;
    Fiber.return ());
  [%expect
    {|
    a_i 12 /workspace_symbol_A/bin/a.ml 22:0 24:7
    a_arr 12 /workspace_symbol_A/bin/a.ml 18:0 18:14
    a_u 12 /workspace_symbol_A/bin/a.ml 16:0 16:15
    a_d 12 /workspace_symbol_A/bin/a.ml 7:0 10:14
    a_b 12 /workspace_symbol_A/bin/a.ml 4:2 4:19
    a_b_t 15 /workspace_symbol_A/bin/a.ml 3:2 3:21
    a_x 12 /workspace_symbol_A/bin/a.ml 0:0 0:11
    |}]
;;

let%expect_test "handles multiple workspaces" =
  let workspace_a, workspace_b = setup_workspaces () in
  build_project workspace_a;
  build_project workspace_b;
  let workspaces = [ workspace_a; workspace_b ] in
  run workspaces (fun client ->
    let* symbols = workspace_symbol client "" in
    print_symbols workspaces symbols;
    Fiber.return ());
  [%expect
    {|
    stack_of_ints 5 /workspace_symbol_A/bin/a.ml 51:0 65:5
    size 6 /workspace_symbol_A/bin/a.ml 64:4 64:38
    peek 6 /workspace_symbol_A/bin/a.ml 62:4 62:34
    pop 6 /workspace_symbol_A/bin/a.ml 57:4 60:12
    push 6 /workspace_symbol_A/bin/a.ml 55:4 55:45
    the_list 12 /workspace_symbol_A/bin/a.ml 53:4 53:40
    Foo 9 /workspace_symbol_A/bin/a.ml 49:0 49:23
    Increment 2 /workspace_symbol_A/bin/a.ml 45:0 47:3
    increment_x 12 /workspace_symbol_A/bin/a.ml 46:2 46:27
    X_int 2 /workspace_symbol_A/bin/a.ml 41:0 43:3
    x 12 /workspace_symbol_A/bin/a.ml 42:2 42:13
    A_Mod 2 /workspace_symbol_A/bin/a.ml 29:0 39:3
    compare 12 /workspace_symbol_A/bin/a.ml 38:2 38:30
    private_mod_fn 12 /workspace_symbol_A/bin/a.ml 36:2 36:33
    t 15 /workspace_symbol_A/bin/a.ml 34:2 34:14
    My_string 2 /workspace_symbol_A/bin/a.ml 27:0 27:25
    StringMap 2 /workspace_symbol_A/bin/a.ml 26:0 26:36
    a_i 12 /workspace_symbol_A/bin/a.ml 22:0 24:7
    a_arr 12 /workspace_symbol_A/bin/a.ml 18:0 18:14
    a_u 12 /workspace_symbol_A/bin/a.ml 16:0 16:15
    user 15 /workspace_symbol_A/bin/a.ml 12:0 14:12
    NotAdmin 9 /workspace_symbol_A/bin/a.ml 14:2 14:12
    Admin 9 /workspace_symbol_A/bin/a.ml 13:2 13:9
    a_d 12 /workspace_symbol_A/bin/a.ml 7:0 10:14
    A_B 2 /workspace_symbol_A/bin/a.ml 2:0 5:3
    a_b 12 /workspace_symbol_A/bin/a.ml 4:2 4:19
    a_b_t 15 /workspace_symbol_A/bin/a.ml 3:2 3:21
    a_x 12 /workspace_symbol_A/bin/a.ml 0:0 0:11
    main_y 12 /workspace_symbol_A/bin/main.ml 0:0 0:22
    vendored_x 12 /workspace_symbol_A/lib/lib.ml 14:0 14:31
    lib_type 12 /workspace_symbol_A/lib/lib.ml 12:0 12:38
    lib_private_fn 12 /workspace_symbol_A/lib/lib.ml 10:0 10:38
    hd 12 /workspace_symbol_A/lib/lib.ml 8:0 8:16
    lib_x 12 /workspace_symbol_A/lib/lib.ml 6:0 6:14
    user 15 /workspace_symbol_A/lib/lib.ml 3:0 5:1
    name 7 /workspace_symbol_A/lib/lib.ml 4:2 4:14
    t 15 /workspace_symbol_A/lib/LibTypes.mli 0:0 0:15
    x 12 /workspace_symbol_A/vendor/vendored_lib.ml 0:0 0:9
    workspace_B 12 /workspace_symbol_B/main.ml 0:0 0:31
    |}]
;;

let%expect_test "shows a notification message if no build directory found" =
  let workspace_a, workspace_b = setup_workspaces () in
  clean_project workspace_a;
  clean_project workspace_b;
  let notification = Fiber.Ivar.create () in
  let on_notification _ = function
    | Lsp.Server_notification.ShowMessage params ->
      let* filled = Fiber.Ivar.peek notification in
      (match filled with
       | Some _ -> Fiber.return ()
       | None -> Fiber.Ivar.fill notification params)
    | _ -> Fiber.return ()
  in
  let workspaces = [ workspace_a; workspace_b ] in
  run ~on_notification workspaces (fun client ->
    let* (_ : SymbolInformation.t list option) = workspace_symbol client "" in
    let* params = Fiber.Ivar.read notification in
    ShowMessageParams.yojson_of_t params |> Test.print_result;
    Fiber.return ());
  [%expect
    {|
    {
      "message": "No build directory found in workspace(s): workspace_symbol_A, workspace_symbol_B",
      "type": 2
    }
    |}]
;;
