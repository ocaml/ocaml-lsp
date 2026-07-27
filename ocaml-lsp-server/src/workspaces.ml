open Import

type t =
  { workspace_folders : WorkspaceFolder.t Map.M(Uri).t option
  ; root_uri : Uri.t option
  ; root_path : string option
  }

let create (ip : InitializeParams.t) =
  let workspace_folders =
    match ip.workspaceFolders with
    | None | Some None -> None
    | Some (Some workspace_folders) ->
      List.map workspace_folders ~f:(fun (ws : WorkspaceFolder.t) -> ws.uri, ws)
      |> Map.of_alist_reduce (module Uri) ~f:(fun _old new_ -> new_)
      |> Option.some
  in
  let root_uri = ip.rootUri in
  let root_path = Option.join ip.rootPath in
  { workspace_folders; root_uri; root_path }
;;

let on_change t { DidChangeWorkspaceFoldersParams.event = { added; removed } } =
  let workspace_folders =
    let init = Option.value t.workspace_folders ~default:(Map.empty (module Uri)) in
    let init =
      List.fold_left removed ~init ~f:(fun acc (a : WorkspaceFolder.t) ->
        Map.remove acc a.uri)
    in
    List.fold_left added ~init ~f:(fun acc (a : WorkspaceFolder.t) ->
      Map.set acc ~key:a.uri ~data:a)
    |> Option.some
  in
  { t with workspace_folders }
;;

let workspace_folders { root_uri; root_path; workspace_folders } =
  match workspace_folders with
  | Some workspace_folders -> Map.data workspace_folders
  | None ->
    (* WorkspaceFolders has the most priority. Then rootUri and finally
       rootPath *)
    (match root_uri, root_path with
     | Some root_uri, _ ->
       [ WorkspaceFolder.create
           ~uri:root_uri
           ~name:(Filename.basename (Uri.to_path root_uri))
       ]
     | None, Some root_path ->
       [ WorkspaceFolder.create
           ~uri:(Uri.of_path root_path)
           ~name:(Filename.basename root_path)
       ]
     | None, None ->
       let cwd = Sys.getcwd () in
       [ WorkspaceFolder.create ~uri:(Uri.of_path cwd) ~name:(Filename.basename cwd) ])
;;
