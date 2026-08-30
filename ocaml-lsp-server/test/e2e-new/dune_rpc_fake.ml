open Test.Import
module Rpc = Dune_rpc.Private

type t =
  { root : string
  ; runtime_dir : string
  ; pid : int
  }

let root t = t.root
let runtime_dir t = t.runtime_dir

let mkdir path =
  match Unix.mkdir path 0o700 with
  | () -> ()
  | exception Unix.Unix_error (Unix.EEXIST, _, _) -> ()
;;

let register runtime_dir dune =
  let env variable =
    if String.equal variable "XDG_RUNTIME_DIR"
    then Some runtime_dir
    else Sys.getenv_opt variable
  in
  let config = Rpc.Registry.Config.create (Xdg.create ~env ()) in
  let watch_dir = Rpc.Registry.Config.watch_dir config in
  mkdir (Filename.dirname watch_dir);
  mkdir watch_dir;
  let (`Caller_should_write file) = Rpc.Registry.Config.register config dune in
  Test.write_file file.path file.contents
;;

let read_packet input ~version =
  match Csexp.input_opt input with
  | Error message -> failwith message
  | Ok None -> None
  | Ok (Some sexp) ->
    (match Rpc.Conv.of_sexp Rpc.Packet.sexp ~version sexp with
     | Ok packet -> Some packet
     | Error error -> Rpc.Conv.dyn_of_error error |> Dyn.to_string |> failwith)
;;

let write_packet output packet =
  Rpc.Conv.to_sexp Rpc.Packet.sexp packet |> Csexp.to_channel output;
  flush output
;;

let response
      (type request response)
      (decl : (request, response) Rpc.Decl.Request.t)
      ~version
      (value : response)
  =
  match
    List.find decl.generations ~f:(fun (generation, _) -> Int.equal generation version)
  with
  | None -> failwith "unsupported fake Dune RPC response version"
  | Some (_, Rpc.Decl.Generation.T conversion) ->
    Rpc.Conv.to_sexp conversion.resp (conversion.downgrade_resp value)
;;

let selected_version menu method_ =
  let name = Rpc.Method.Name.to_string method_ in
  match List.Assoc.find menu name ~equal:String.equal with
  | Some version -> version
  | None -> failwith ("Dune RPC method was not negotiated: " ^ name)
;;

let serve connection ~diagnostics ~progress =
  let input = Unix.in_channel_of_descr connection in
  let output = Unix.out_channel_of_descr connection in
  let latest = Rpc.Version.latest in
  let initialize_id, initialize =
    match read_packet input ~version:latest with
    | Some (Rpc.Packet.Request (id, call)) ->
      (match Rpc.Initialize.Request.of_call call ~version:latest with
       | Ok initialize -> id, initialize
       | Error _ -> failwith "invalid Dune RPC initialize request")
    | Some _ | None -> failwith "expected Dune RPC initialize request"
  in
  write_packet
    output
    (Rpc.Packet.Response
       ( initialize_id
       , Ok (Rpc.Initialize.Response.create () |> Rpc.Initialize.Response.to_response) ));
  let dune_version = Rpc.Initialize.Request.dune_version initialize in
  let menu_id, offered =
    match read_packet input ~version:dune_version with
    | Some (Rpc.Packet.Request (id, call)) ->
      (match Rpc.Version_negotiation.Request.of_call call ~version:dune_version with
       | Ok (Rpc.Version_negotiation.Request.Menu offered) -> id, offered
       | Error _ -> failwith "invalid Dune RPC version negotiation request")
    | Some _ | None -> failwith "expected Dune RPC version negotiation request"
  in
  let selected =
    List.map offered ~f:(fun (method_, versions) ->
      let version = List.fold_left versions ~init:0 ~f:Int.max in
      method_, version)
  in
  write_packet
    output
    (Rpc.Packet.Response
       ( menu_id
       , Ok
           (Rpc.Version_negotiation.Response.create selected
            |> Rpc.Version_negotiation.Response.to_response) ));
  let menu =
    List.map selected ~f:(fun (method_, version) ->
      Rpc.Method.Name.to_string method_, version)
  in
  let diagnostic_decl = Rpc.Procedures.Poll.poll Rpc.Procedures.Poll.diagnostic in
  let progress_decl = Rpc.Procedures.Poll.poll Rpc.Procedures.Poll.progress in
  let diagnostics = ref diagnostics in
  let progress = ref progress in
  let next queue =
    match !queue with
    | [] -> None
    | value :: rest ->
      queue := rest;
      Some value
  in
  let rec loop () =
    match read_packet input ~version:dune_version with
    | None -> ()
    | Some (Rpc.Packet.Notification _) -> loop ()
    | Some (Rpc.Packet.Response _) -> failwith "unexpected response from Dune RPC client"
    | Some (Rpc.Packet.Request (id, call)) ->
      if Rpc.Method.Name.equal call.method_ diagnostic_decl.decl.method_
      then (
        let version = selected_version menu call.method_ in
        let payload = response diagnostic_decl ~version (next diagnostics) in
        write_packet output (Rpc.Packet.Response (id, Ok payload));
        loop ())
      else if Rpc.Method.Name.equal call.method_ progress_decl.decl.method_
      then (
        let version = selected_version menu call.method_ in
        let payload = response progress_decl ~version (next progress) in
        write_packet output (Rpc.Packet.Response (id, Ok payload));
        loop ())
      else
        failwith ("unexpected Dune RPC request: " ^ Rpc.Method.Name.to_string call.method_)
  in
  loop ()
;;

let start ?(diagnostics = fun _root -> []) ?(progress = []) name =
  let temp = Test.temp_dir ("ocamllsp-fake-dune-" ^ name ^ "-") in
  let root = Filename.concat temp "workspace" in
  let runtime_dir = Filename.concat temp "runtime" in
  mkdir root;
  mkdir runtime_dir;
  Test.write_file (Filename.concat root "dune-project") "(lang dune 3.18)\n";
  let socket_path = Filename.concat temp "rpc.sock" in
  let listener = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind listener (Unix.ADDR_UNIX socket_path);
  Unix.listen listener 1;
  let diagnostics = diagnostics root in
  let pid =
    match Unix.fork () with
    | 0 ->
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      let connection, _ = Unix.accept listener in
      Unix.close listener;
      (match serve connection ~diagnostics ~progress with
       | () ->
         Unix.close connection;
         Unix._exit 0
       | exception Sys_error _ ->
         Unix.close connection;
         Unix._exit 0
       | exception exn ->
         Format.eprintf "fake Dune RPC server: %s@." (Printexc.to_string exn);
         Unix.close connection;
         Unix._exit 2)
    | pid -> pid
  in
  Unix.close listener;
  let dune = Rpc.Registry.Dune.create ~where:(`Unix socket_path) ~root ~pid in
  register runtime_dir dune;
  { root; runtime_dir; pid }
;;

let stop t =
  (match Unix.kill t.pid Sys.sigterm with
   | () -> ()
   | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ());
  match Test.waitpid t.pid with
  | Unix.WEXITED 0 -> ()
  | Unix.WSIGNALED signal when signal = Sys.sigterm -> ()
  | status ->
    failwith
      (Printf.sprintf
         "fake Dune RPC server failed: %s"
         (match status with
          | Unix.WEXITED code -> Printf.sprintf "exit %d" code
          | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
          | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal))
;;
