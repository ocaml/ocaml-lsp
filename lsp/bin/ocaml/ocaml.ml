open! Import

let of_typescript _ = []

let name = "gprotocol"

let mli = name ^ ".mli"
let ml = name ^ ".ml"

let output _ =
  Io.String_path.write_file mli "";
  Io.String_path.write_file ml "";
