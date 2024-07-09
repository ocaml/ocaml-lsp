type t =
  { content_length : int
  ; content_type : string
  }

let content_type t = t.content_type
let content_length t = t.content_length

module Private = struct
  module Key = struct
    let content_length = "Content-Length"
    let content_type = "Content-Type"
  end
end

open Private

let crlf = "\r\n"

let to_string { content_length; content_type } =
  let b = Buffer.create 64 in
  let add = Buffer.add_string b in
  let line k v =
    add k;
    add ": ";
    add v;
    add crlf
  in
  line Key.content_length (string_of_int content_length);
  line Key.content_type content_type;
  add crlf;
  Buffer.contents b
;;

let default_content_type = "application/vscode-jsonrpc; charset=utf-8"

let create ?(content_type = default_content_type) ~content_length () =
  { content_length; content_type }
;;
