open Import

module Uri_map = Map.Make (struct
    include Uri

    let compare x y = Ordering.of_int (compare x y)
  end)

type t =
  { workspace_folders : WorkspaceFolder.t Uri_map.t option
  ; root_uri : Uri.t option
  ; root_path : string option
  }

let create (ip : InitializeParams.t) =
  let workspace_folders =
    match ip.workspaceFolders with
    | None | Some None -> None
    | Some (Some workspace_folders) ->
      Uri_map.of_list_map_exn workspace_folders ~f:(fun (ws : WorkspaceFolder.t) ->
        ws.uri, ws)
      |> Option.some
  in
  let root_uri = ip.rootUri in
  let root_path =
    match ip.rootPath with
    | None -> None
    | Some s -> s
  in
  { workspace_folders; root_uri; root_path }
;;

let on_change t { DidChangeWorkspaceFoldersParams.event = { added; removed } } =
  assert (t.workspace_folders <> None);
  let workspace_folders =
    let init = Option.value t.workspace_folders ~default:Uri_map.empty in
    let init =
      List.fold_left removed ~init ~f:(fun acc (a : WorkspaceFolder.t) ->
        Uri_map.remove acc a.uri)
    in
    List.fold_left added ~init ~f:(fun acc (a : WorkspaceFolder.t) ->
      Uri_map.set acc a.uri a)
    |> Option.some
  in
  { t with workspace_folders }
;;

let workspace_folders { root_uri; root_path; workspace_folders } =
  match workspace_folders with
  | Some s -> Uri_map.values s
  | None ->
    (* WorkspaceFolders has the most priority. Then rootUri and finally
       rootPath *)
    (match workspace_folders, root_uri, root_path with
     | Some workspace_folders, _, _ -> Uri_map.values workspace_folders
     | _, Some root_uri, _ ->
       [ WorkspaceFolder.create
           ~uri:root_uri
           ~name:(Filename.basename (Uri.to_path root_uri))
       ]
     | _, _, Some root_path ->
       [ WorkspaceFolder.create
           ~uri:(Uri.of_path root_path)
           ~name:(Filename.basename root_path)
       ]
     | _ ->
       let cwd = Sys.getcwd () in
       [ WorkspaceFolder.create ~uri:(Uri.of_path cwd) ~name:(Filename.basename cwd) ])
;;
