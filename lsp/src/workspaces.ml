open Import
open Types
module Uri_map = Map.Make (DocumentUri)

type t =
  { workspace_folders : WorkspaceFolder.t Uri_map.t option
  ; root_uri : DocumentUri.t option
  ; root_path : string option
  }

let map_of_workspace_folders workspace_folders =
  List.fold_left
    workspace_folders
    ~init:Uri_map.empty
    ~f:(fun acc (workspace_folder : WorkspaceFolder.t) ->
      Uri_map.add ~key:workspace_folder.uri ~data:workspace_folder acc)
;;

let create (params : InitializeParams.t) =
  let workspace_folders =
    match params.workspaceFolders with
    | None | Some None -> None
    | Some (Some workspace_folders) -> Some (map_of_workspace_folders workspace_folders)
  in
  let root_path =
    match params.rootPath with
    | None -> None
    | Some root_path -> root_path
  in
  { workspace_folders; root_uri = params.rootUri; root_path }
;;

let on_change t (params : DidChangeWorkspaceFoldersParams.t) =
  let workspace_folders =
    let init = Option.value t.workspace_folders ~default:Uri_map.empty in
    let init =
      List.fold_left
        params.event.removed
        ~init
        ~f:(fun acc (workspace_folder : WorkspaceFolder.t) ->
          Uri_map.remove workspace_folder.uri acc)
    in
    List.fold_left
      params.event.added
      ~init
      ~f:(fun acc (workspace_folder : WorkspaceFolder.t) ->
        Uri_map.add ~key:workspace_folder.uri ~data:workspace_folder acc)
  in
  { t with workspace_folders = Some workspace_folders }
;;

let workspace_folders { root_uri; root_path; workspace_folders } =
  match workspace_folders with
  | Some workspace_folders -> List.map (Uri_map.bindings workspace_folders) ~f:snd
  | None ->
    (* Workspace folders have priority over [rootUri], then [rootPath]. *)
    (match root_uri, root_path with
     | Some root_uri, _ ->
       [ WorkspaceFolder.create
           ~uri:root_uri
           ~name:(Filename.basename (DocumentUri.to_path root_uri))
       ]
     | None, Some root_path ->
       [ WorkspaceFolder.create
           ~uri:(DocumentUri.of_path root_path)
           ~name:(Filename.basename root_path)
       ]
     | None, None ->
       let cwd = Sys.getcwd () in
       [ WorkspaceFolder.create
           ~uri:(DocumentUri.of_path cwd)
           ~name:(Filename.basename cwd)
       ])
;;

let normalize_directory path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  (* [Filename.dirname] treats the final path component as a basename, even when
     [path] has a trailing separator. Appending [.] marks [path] as a directory;
     taking its dirname then strips trailing separators while preserving roots. *)
  Filename.concat path Filename.current_dir_name |> Filename.dirname
;;

let equal_path =
  if Sys.win32
  then fun x y -> String.equal (String.lowercase_ascii x) (String.lowercase_ascii y)
  else String.equal
;;

let find_workspace_folder t uri =
  let roots =
    workspace_folders t
    |> List.map ~f:(fun (folder : WorkspaceFolder.t) ->
      normalize_directory (DocumentUri.to_path folder.uri), folder)
  in
  let rec loop directory =
    match List.find_opt roots ~f:(fun (root, _) -> equal_path root directory) with
    | Some (_, folder) -> Some folder
    | None ->
      let parent = Filename.dirname directory in
      if equal_path parent directory then None else loop parent
  in
  DocumentUri.to_path uri |> Filename.dirname |> normalize_directory |> loop
;;
