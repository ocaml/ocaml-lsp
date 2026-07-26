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

module Sequence_channel = struct
  type input = [ `Line of string | `Body of string ] list ref
  type output = unit

  let read_line input =
    match !input with
    | [] -> None
    | `Line line :: rest ->
      input := rest;
      Some line
    | `Body _ :: _ -> failwith "expected a header line"
  ;;

  let read_exactly input length =
    match !input with
    | `Body body :: rest when String.length body = length ->
      input := rest;
      Some body
    | `Body _ :: _ -> None
    | [] | `Line _ :: _ -> failwith "expected a packet body"
  ;;

  let write () _ = ()
end

module Sequence_framing = Lsp.Io.Make (Immediate) (Sequence_channel)

let print_packet label packet =
  Printf.printf
    "%s:\n%s\n"
    label
    (Yojson.Safe.pretty_to_string ~std:false (Jsonrpc.Packet.yojson_of_t packet))
;;

let print_requested_bytes label requested_bytes =
  Printf.printf
    "%s: [%s]\n"
    label
    (String.concat ", " (List.map string_of_int requested_bytes))
;;

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
  assert (parsed = packet);
  print_packet "decoded packet" parsed;
  [%expect
    {|
    decoded packet:
    { "method": "notify", "jsonrpc": "2.0" } |}]
;;

let check_read_error label input =
  match Framing.read input with
  | _ -> Printf.printf "%s: accepted\n" label
  | exception Lsp.Io.Error message -> Printf.printf "%s: %s\n" label message
;;

let%expect_test "LSP framing handles EOF and malformed packet bodies" =
  (match Framing.read (Channel.input []) with
   | None -> print_endline "clean EOF: end of stream"
   | Some packet -> print_packet "clean EOF returned a packet" packet);
  let check label body =
    let input =
      Channel.input ~body [ Printf.sprintf "Content-Length: %d" (String.length body); "" ]
    in
    match Framing.read input with
    | _ -> Printf.printf "%s: accepted\n" label
    | exception Yojson.Json_error _ -> Printf.printf "%s: invalid JSON\n" label
    | exception Jsonrpc.Json.Of_json _ -> Printf.printf "%s: invalid packet\n" label
  in
  check "truncated JSON" "{";
  check "scalar packet" "null";
  [%expect
    {|
    clean EOF: end of stream
    truncated JSON: invalid JSON
    scalar packet: invalid packet |}]
;;

let%expect_test "LSP framing rejects invalid lengths and truncated bodies" =
  check_read_error "missing" (Channel.input [ "Content-Type: application/json"; "" ]);
  check_read_error "nonnumeric" (Channel.input [ "Content-Length: many"; "" ]);
  let negative = Channel.input [ "Content-Length: -1"; "" ] in
  check_read_error "negative" negative;
  print_requested_bytes "negative body reads" negative.requested_bytes;
  check_read_error "truncated" (Channel.input ~body:"{}" [ "Content-Length: 3"; "" ]);
  [%expect
    {|
    missing: content length absent
    nonnumeric: Content-Length is invalid
    negative: Content-Length is invalid
    negative body reads: []
    truncated: unable to read json |}]
;;

let%expect_test "LSP framing defines duplicate and overflowing length behavior" =
  let packet =
    Jsonrpc.Packet.Notification
      (Jsonrpc.Notification.create ~method_:"duplicate-length" ())
  in
  let body = Jsonrpc.Packet.yojson_of_t packet |> Yojson.Safe.to_string in
  let body_length = String.length body in
  let duplicate =
    Channel.input
      ~body
      [ Printf.sprintf "Content-Length: %d" body_length
      ; Printf.sprintf "content-length: %d" (body_length + 10)
      ; ""
      ]
  in
  let parsed = Framing.read duplicate |> Option.get in
  print_packet "duplicate length decoded packet" parsed;
  print_requested_bytes "duplicate length body reads" duplicate.requested_bytes;
  let overflow = string_of_int max_int ^ "0" in
  check_read_error
    "overflowing length"
    (Channel.input [ "Content-Length: " ^ overflow; "" ]);
  [%expect
    {|
    duplicate length decoded packet:
    { "method": "duplicate-length", "jsonrpc": "2.0" }
    duplicate length body reads: [45]
    overflowing length: Content-Length is invalid
    |}]
;;

let%expect_test "LSP framing reads back-to-back packets independently" =
  let first =
    Jsonrpc.Packet.Notification (Jsonrpc.Notification.create ~method_:"first" ())
  in
  let second =
    Jsonrpc.Packet.Response
      (Jsonrpc.Response.ok (`String "second") (`Assoc [ "value", `Int 2 ]))
  in
  let frame packet =
    let body = Jsonrpc.Packet.yojson_of_t packet |> Yojson.Safe.to_string in
    [ `Line (Printf.sprintf "Content-Length: %d" (String.length body))
    ; `Line ""
    ; `Body body
    ]
  in
  let input = ref (frame first @ frame second) in
  let first = Sequence_framing.read input |> Option.get in
  let second = Sequence_framing.read input |> Option.get in
  print_packet "first packet" first;
  print_packet "second packet" second;
  (match Sequence_framing.read input with
   | None -> print_endline "stream: end of input"
   | Some packet -> print_packet "unexpected trailing packet" packet);
  [%expect
    {|
    first packet:
    { "method": "first", "jsonrpc": "2.0" }
    second packet:
    { "id": "second", "jsonrpc": "2.0", "result": { "value": 2 } }
    stream: end of input
    |}]
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
    Channel.input ~body [ Printf.sprintf "Content-Length: %d" (String.length body); "" ]
  in
  let parsed = Framing.read input |> Option.get in
  assert (parsed = packet);
  print_packet "decoded packet" parsed;
  print_requested_bytes "body reads" input.requested_bytes;
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
      "Content-Length: %d\r\n\
       Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\
       \r\n"
      (String.length written_body)
  in
  assert (String.equal header expected_header);
  Printf.printf "written header: %S\n" header;
  let written_packet =
    Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string written_body)
  in
  assert (written_packet = packet);
  print_packet "written packet" written_packet;
  [%expect
    {|
    decoded packet:
    {
      "params": { "message": "hello 😀" },
      "method": "window/logMessage",
      "jsonrpc": "2.0"
    }
    body reads: [80]
    written header: "Content-Length: 80\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n"
    written packet:
    {
      "params": { "message": "hello 😀" },
      "method": "window/logMessage",
      "jsonrpc": "2.0"
    }
    |}]
;;
