open Lsp
open Types

let folder path =
  WorkspaceFolder.create ~uri:(Uri.of_path path) ~name:(Filename.basename path)
;;

let initialize ?rootPath ?rootUri ?workspaceFolders () =
  InitializeParams.create
    ~capabilities:(ClientCapabilities.create ())
    ?rootPath
    ?rootUri
    ?workspaceFolders
    ()
;;

let print_folders workspace_folders =
  Workspaces.workspace_folders workspace_folders
  |> List.iter (fun (folder : WorkspaceFolder.t) ->
    Printf.printf "%s %s\n" folder.name (Uri.to_path folder.uri))
;;

let%expect_test "workspace folders are updated" =
  let first = folder "/workspace/first" in
  let second = folder "/workspace/second" in
  let third = folder "/workspace/third" in
  let workspaces =
    Workspaces.create (initialize ~workspaceFolders:(Some [ first; second ]) ())
  in
  print_folders workspaces;
  [%expect
    {|
    first /workspace/first
    second /workspace/second
    |}];
  let event = WorkspaceFoldersChangeEvent.create ~added:[ third ] ~removed:[ first ] in
  let workspaces =
    Workspaces.on_change workspaces (DidChangeWorkspaceFoldersParams.create ~event)
  in
  print_folders workspaces;
  [%expect
    {|
    second /workspace/second
    third /workspace/third
    |}]
;;

let%expect_test "workspace folder fallbacks" =
  let root_uri = Uri.of_path "/workspace/root-uri" in
  let root_uri_workspaces = Workspaces.create (initialize ~rootUri:root_uri ()) in
  print_folders root_uri_workspaces;
  [%expect {| root-uri /workspace/root-uri |}];
  let root_path_workspaces =
    Workspaces.create (initialize ~rootPath:(Some "/workspace/root-path") ())
  in
  print_folders root_path_workspaces;
  let explicit_workspaces =
    Workspaces.create
      (initialize
         ~rootUri:root_uri
         ~rootPath:(Some "/workspace/root-path")
         ~workspaceFolders:(Some [ folder "/workspace/explicit" ])
         ())
  in
  print_folders explicit_workspaces;
  [%expect
    {|
    root-path /workspace/root-path
    explicit /workspace/explicit
    |}]
;;
