module Metamodel = Lsp_gen.Metamodel

type message_direction =
  | Client_to_server
  | Server_to_client
  | Both

type call =
  { method_ : string
  ; message_direction : message_direction
  }

let calls name json =
  let open Yojson.Safe.Util in
  json
  |> member name
  |> to_list
  |> List.map (fun json ->
    let method_ = json |> member "method" |> to_string in
    let message_direction =
      match json |> member "messageDirection" |> to_string with
      | "clientToServer" -> Client_to_server
      | "serverToClient" -> Server_to_client
      | "both" -> Both
      | direction -> failwith (Printf.sprintf "unknown message direction %S" direction)
    in
    { method_; message_direction })
;;

(* Parameters are deliberately omitted. Recognized methods may fail parameter decoding,
   while methods missing from dispatch reach an [Unknown*] constructor. *)
let client_request_is_known method_ =
  let request = Jsonrpc.Request.create ~id:(`Int 0) ~method_ () in
  match Lsp.Client_request.of_jsonrpc request with
  | Ok (Lsp.Client_request.E (Lsp.Client_request.UnknownRequest _)) -> false
  | Ok _ | Error _ -> true
;;

let server_request_is_known method_ =
  let request = Jsonrpc.Request.create ~id:(`Int 0) ~method_ () in
  match Lsp.Server_request.of_jsonrpc request with
  | Ok (Lsp.Server_request.E (Lsp.Server_request.UnknownRequest _)) -> false
  | Ok _ | Error _ -> true
;;

let client_notification_is_known method_ =
  let notification = Jsonrpc.Notification.create ~method_ () in
  match Lsp.Client_notification.of_jsonrpc notification with
  | Ok (Lsp.Client_notification.UnknownNotification _) -> false
  | Ok _ | Error _ -> true
;;

let server_notification_is_known method_ =
  let notification = Jsonrpc.Notification.create ~method_ () in
  match Lsp.Server_notification.of_jsonrpc notification with
  | Ok (Lsp.Server_notification.UnknownNotification _) -> false
  | Ok _ | Error _ -> true
;;

let check_dispatch_coverage json =
  let unknown = ref [] in
  let check label is_known method_ =
    if not (is_known method_) then unknown := (label, method_) :: !unknown
  in
  let check_call ~client_to_server ~server_to_client call =
    let client_label, client_is_known = client_to_server in
    let server_label, server_is_known = server_to_client in
    match call.message_direction with
    | Client_to_server -> check client_label client_is_known call.method_
    | Server_to_client -> check server_label server_is_known call.method_
    | Both ->
      check client_label client_is_known call.method_;
      check server_label server_is_known call.method_
  in
  List.iter
    (check_call
       ~client_to_server:("client-to-server request", client_request_is_known)
       ~server_to_client:("server-to-client request", server_request_is_known))
    (calls "requests" json);
  List.iter
    (check_call
       ~client_to_server:("client-to-server notification", client_notification_is_known)
       ~server_to_client:("server-to-client notification", server_notification_is_known))
    (calls "notifications" json);
  match List.rev !unknown with
  | [] -> ()
  | unknown ->
    let methods =
      List.map
        (fun (direction, method_) -> Printf.sprintf "- %s: %s" direction method_)
        unknown
      |> String.concat "\n"
    in
    failwith ("metamodel methods parsed as unknown:\n" ^ methods)
;;

let file = Sys.argv.(1)

let () =
  let read = open_in file in
  let s = really_input_string read (in_channel_length read) in
  let json = Yojson.Safe.from_string s in
  let (_ : Metamodel.t) = Metamodel.t json in
  check_dispatch_coverage json;
  close_in read
;;
