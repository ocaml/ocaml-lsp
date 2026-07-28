let find_program program =
  let separator = if Sys.win32 then ';' else ':' in
  let executable = if Sys.win32 then program ^ ".exe" else program in
  let path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  String.split_on_char separator path
  |> List.find_map (fun directory ->
    let candidate = Filename.concat directory executable in
    if Sys.file_exists candidate then Some candidate else None)
  |> Option.get
;;

module Process = struct
  type t =
    { pid : int
    ; input : in_channel
    ; output : out_channel
    }

  let start () =
    let child_stdin, output = Unix.pipe ~cloexec:true () in
    let input, child_stdout = Unix.pipe ~cloexec:true () in
    let program = find_program "ocp-indent-rpc" in
    let pid =
      Unix.create_process program [| program |] child_stdin child_stdout Unix.stderr
    in
    Unix.close child_stdin;
    Unix.close child_stdout;
    let input = Unix.in_channel_of_descr input in
    let output = Unix.out_channel_of_descr output in
    set_binary_mode_in input true;
    set_binary_mode_out output true;
    { pid; input; output }
  ;;

  let stop { pid; input; output } =
    close_out_noerr output;
    let _, status = Unix.waitpid [] pid in
    close_in_noerr input;
    match status with
    | Unix.WEXITED 0 -> ()
    | WEXITED code -> failwith (Printf.sprintf "ocp-indent-rpc exited with %d" code)
    | WSIGNALED signal ->
      failwith (Printf.sprintf "ocp-indent-rpc was killed by signal %d" signal)
    | WSTOPPED signal ->
      failwith (Printf.sprintf "ocp-indent-rpc was stopped by signal %d" signal)
  ;;

  let with_process f =
    let process = start () in
    Fun.protect (fun () -> f process) ~finally:(fun () -> stop process)
  ;;

  let send { output; _ } request =
    Csexp.to_channel output request;
    flush output
  ;;

  let receive { input; _ } =
    match Csexp.input input with
    | Ok response -> response
    | Error message -> failwith message
  ;;
end

let rec pp_sexp ppf = function
  | Csexp.Atom atom -> Format.fprintf ppf "%S" atom
  | List sexps ->
    Format.fprintf
      ppf
      "(@[<hov>%a@])"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_sexp)
      sexps
;;

let print_response process = Format.printf "%a@." pp_sexp (Process.receive process)

let indent ?(syntaxes = []) process ~path ~line source =
  let request =
    Csexp.List
      [ Atom "indent"
      ; Atom path
      ; Atom (string_of_int line)
      ; Atom source
      ; List (List.map (fun syntax -> Csexp.Atom syntax) syntaxes)
      ]
  in
  Process.send process request;
  print_response process
;;

let test_path = Filename.concat (Filename.get_temp_dir_name ()) "test.ml"

let%expect_test "negotiates the protocol and serves multiple requests" =
  Process.with_process (fun process ->
    print_response process;
    indent process ~path:test_path ~line:2 "let f =\n\n";
    indent process ~path:test_path ~line:1 "let x = 1\n");
  [%expect
    {|
    ("version" "1")
    ("ok" "2")
    ("ok" "0")
    |}]
;;

let%expect_test "reloads .ocp-indent and ignores plugin directives" =
  let dir = Filename.temp_file "ocp-indent-rpc-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let config = Filename.concat dir ".ocp-indent" in
  let path = Filename.concat dir "test.ml" in
  let nested_dir = Filename.concat dir "nested" in
  Unix.mkdir nested_dir 0o700;
  let nested_config = Filename.concat nested_dir ".ocp-indent" in
  let nested_path = Filename.concat nested_dir "test.ml" in
  let write_config path base =
    let output = open_out path in
    Fun.protect
      (fun () -> Printf.fprintf output "base=%d\nload=missing-ocp-indent-plugin\n" base)
      ~finally:(fun () -> close_out output)
  in
  Fun.protect
    (fun () ->
       write_config config 4;
       Process.with_process (fun process ->
         ignore (Process.receive process : Csexp.t);
         indent process ~path ~line:2 "let f =\n\n";
         write_config config 6;
         indent process ~path ~line:2 "let f =\n\n";
         write_config nested_config 8;
         indent process ~path:nested_path ~line:2 "let f =\n\n"))
    ~finally:(fun () ->
      if Sys.file_exists nested_config then Sys.remove nested_config;
      if Sys.file_exists config then Sys.remove config;
      Unix.rmdir nested_dir;
      Unix.rmdir dir);
  [%expect
    {|
    ("ok" "4")
    ("ok" "6")
    ("ok" "8")
    |}]
;;

let%expect_test "supports requested syntax extensions" =
  let source = "{\nlet x = 1\n}\nrule token = parse\n| eof { () }\n| _ {\n\n}\n" in
  Process.with_process (fun process ->
    ignore (Process.receive process : Csexp.t);
    indent process ~path:test_path ~line:7 source;
    indent process ~syntaxes:[ "mll" ] ~path:test_path ~line:7 source);
  [%expect
    {|
    ("ok" "2")
    ("ok" "4")
    |}]
;;

let%expect_test "reports malformed requests and remains usable" =
  Process.with_process (fun process ->
    ignore (Process.receive process : Csexp.t);
    Process.send process (Csexp.List [ Atom "invalid" ]);
    print_response process;
    indent process ~path:test_path ~line:2 "let f =\n\n");
  [%expect
    {|
    ("error" "Invalid_argument(\"invalid request\")")
    ("ok" "2")
    |}]
;;
