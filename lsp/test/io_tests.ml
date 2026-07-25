module Immediate = struct
  type 'a t = 'a

  let return x = x
  let raise exn = Stdlib.raise exn

  module O = struct
    let ( let+ ) x f = f x
    let ( let* ) x f = f x
  end
end

module Channel = struct
  type input =
    { mutable lines : string list
    ; mutable body : string option
    ; mutable requested_bytes : int list
    }

  type output = string list list ref

  let input ?body lines = { lines; body; requested_bytes = [] }

  let read_line input =
    match input.lines with
    | [] -> None
    | line :: lines ->
      input.lines <- lines;
      Some line
  ;;

  let read_exactly input length =
    input.requested_bytes <- length :: input.requested_bytes;
    match input.body with
    | None -> None
    | Some body when String.length body = length -> Some body
    | Some _ -> None
  ;;

  let write output chunks = output := chunks :: !output
end

module Framing = Lsp.Io.Make (Immediate) (Channel)

let%expect_test "LSP headers are parsed case-insensitively" =
  let packet =
    Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:"notify" ())
  in
  let body = Jsonrpc.Packet.yojson_of_t packet |> Yojson.Safe.to_string in
  let input =
    Channel.input
      ~body
      [ "Ignored: header"
      ; Printf.sprintf "cOnTeNt-LeNgTh: %d" (String.length body)
      ; "CONTENT-TYPE: application/json"
      ; "\r"
      ]
  in
  let parsed = Framing.read input |> Option.get in
  Printf.printf "read packet: %b\n" (parsed = packet);
  [%expect {| read packet: true |}]
;;

let%expect_test "LSP framing reads and writes JSON-RPC packets" =
  let packet =
    Jsonrpc.Packet.Notification
      (Jsonrpc.Notification.create
         ~method_:"window/logMessage"
         ~params:(`Assoc [ "message", `String "hello 😀" ])
         ())
  in
  let body = Jsonrpc.Packet.yojson_of_t packet |> Yojson.Safe.to_string in
  let input =
    Channel.input
      ~body
      [ Printf.sprintf "Content-Length: %d" (String.length body); "" ]
  in
  let parsed = Framing.read input |> Option.get in
  Printf.printf "read round trip: %b\n" (parsed = packet);
  Printf.printf
    "requested exact body length: %b\n"
    (List.hd input.requested_bytes = String.length body);
  let output = ref [] in
  Framing.write output packet;
  let chunks = List.hd !output in
  let header, written_body =
    match chunks with
    | [ header; body ] -> header, body
    | _ -> failwith "expected a header and body"
  in
  let expected_header =
    Printf.sprintf
      "Content-Length: %d\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n"
      (String.length written_body)
  in
  Printf.printf "write content length: %b\n" (String.equal header expected_header);
  Printf.printf
    "write round trip: %b\n"
    (Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string written_body) = packet);
  [%expect
    {|
    read round trip: true
    requested exact body length: true
    write content length: true
    write round trip: true |}]
;;
