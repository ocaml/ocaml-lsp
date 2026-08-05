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

let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o700

type workspace =
  { name : string
  ; path : string
  ; folder : WorkspaceFolder.t
  }

let create_workspace root name =
  let path = Filename.concat root name in
  mkdir path;
  let uri = DocumentUri.of_path path in
  { name; path; folder = WorkspaceFolder.create ~name ~uri }
;;

let setup_workspaces () =
  let root = Test.temp_dir "ocamllsp-workspace-symbol-" in
  let workspace_a = create_workspace root "workspace_symbol_A" in
  let workspace_b = create_workspace root "workspace_symbol_B" in
  let write workspace rel content =
    Test.write_file (Filename.concat workspace.path rel) content
  in
  mkdir (Filename.concat workspace_a.path "bin");
  mkdir (Filename.concat workspace_a.path "lib");
  mkdir (Filename.concat workspace_a.path "vendor");
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
      Option.some_if (String.is_prefix path ~prefix:workspace.path) workspace.path)
  in
  let relative_path =
    match workspace_path with
    | None -> path
    | Some workspace_path ->
      let parent = Filename.dirname workspace_path in
      String.drop_prefix path (String.length parent)
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

let run ?on_notification ?capabilities workspaces f =
  let handler = Client.Handler.make ?on_notification () in
  let workspaceFolders =
    Some (List.map workspaces ~f:(fun workspace -> workspace.folder))
  in
  Test.run_initialized ~handler ?capabilities ~workspaceFolders (fun client ->
    let* () = f client in
    Test.shutdown_client client)
;;
